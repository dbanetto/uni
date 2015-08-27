/*****************************************************************************
 * This implements minimal DV - based on the minimal changes to flooding1.c  *
 *                                                                           *
 * Kris Bubendorfer 2015                                                     *
 *****************************************************************************/

#include <cnet.h>
#include <stdlib.h>
#include <string.h>
#include "nl_table.h"
#include "dll_basic.h"

#define MAXHOPS 8
#define INF 16          /* This is going to be our value for infinity */
#define NL_TIMER_INIT 1 /* Boot timer */


typedef enum {NL_DATA, NL_ACK, NL_ROUTE}   NL_PACKETKIND;

typedef struct {
    CnetAddr        src;
    CnetAddr        dest;
    NL_PACKETKIND   kind;   /* only ever NL_DATA or NL_ACK */
    int             seqno;      /* 0, 1, 2, ... */
    int             hopcount;
    size_t          length;     /* the length of the msg portion only */
    char            msg[MAX_MESSAGE_SIZE];
} NL_PACKET;

#define PACKET_HEADER_SIZE  (sizeof(NL_PACKET) - MAX_MESSAGE_SIZE)
#define PACKET_SIZE(p)	    (PACKET_HEADER_SIZE + p.length)

// We don't need anything fancy in the routing table, just a 2D array of int.

static int **rtable = NULL;  /* Declare this node's routing table */
static int rtable_size;


static void send_packet(char *packet, size_t length)
{
    // If we are here, then there must be a route, as the application has been
    // enabled.  This would not be true if we were dealing with broken links etc.

    // What we do here is look up the routing table, find the minimum cost to a
    // destination, and send it out on that link.

    // For this simple version, only applications for which there is a route found
    // will send anyway.  This precludes links breaking etc - which is for 300 lvl.

    // Your code here
    NL_PACKET *p = (NL_PACKET*)(packet);
    printf("Packet CNET dest: %i\n", p->dest);
    printf("Packet CNET src: %i\n", p->src);

    int link = 0, i, min = INF;
    for (i = 0; i < nodeinfo.nlinks+1; i++) {
        if (rtable[p->dest][i] < min) {
            min = rtable[p->dest][i];
            link = i;
        }
    }

    printf("Packet min cost: %i\n", min);
    printf("Packet link: %i\n", link);

    CHECK(down_to_datalink(link, packet, length));
}


void send_table()
{
    // OK, this is called in response to 2 seperate conditions,
    // 1.  When EV_TIMER1 fires, or
    // 2.  When this node has a triggered update.
    // in both cases we don't care - we do the same thing.
    // Note: this is a simple implementation - we don't do poisoned reverse etc. (intentionally)

    // What we do is, find the least costs and send this down each link.

    // Your code here.
    // ensure you use the right type of packet - see NL_PACKETKIND
    int *table = (int*)malloc(sizeof(int) * rtable_size);
    int i, link;
    for(i = 0; i < rtable_size; i++) {
        table[i] = INF;
        for(link = 0; link <= nodeinfo.nlinks; link++) {
            if (rtable[i][link] < table[i]) {
                table[i] = rtable[i][link];
            }
        }
    }

    for(link = 1; link <= nodeinfo.nlinks; link++) {
        NL_PACKET	p;
        p.length = sizeof(int) * rtable_size + 1;
        p.src = nodeinfo.address;
        p.dest = link;
        p.kind = NL_ROUTE;
        p.hopcount	= 0;
        p.seqno	= NL_nextpackettosend(p.src);

        memcpy(&p.msg, table, p.length);

        CHECK(down_to_datalink(link, (char*)(&p), PACKET_SIZE(p)));

    }
    free(table);

}

/*  down_to_network() RECEIVES NEW MESSAGES FROM THE APPLICATION LAYER AND
    PREPARES THEM FOR TRANSMISSION TO OTHER NODES.
    */

static EVENT_HANDLER(down_to_network)
{
    NL_PACKET	p;

    p.length	= sizeof(p.msg);
    CHECK(CNET_read_application(&p.dest, p.msg, &p.length));
    CHECK(CNET_disable_application(p.dest));

    p.src	= nodeinfo.address;
    p.kind	= NL_DATA;
    p.hopcount	= 0;
    p.seqno	= NL_nextpackettosend(p.dest);

    send_packet((char *)&p, PACKET_SIZE(p));
}

static void update_routing_table(char *packet, size_t length, int arrived_on)
{

    // Now, the sender encoded their routing table int the packet
    // extract the info and update our table.

    // We will be using triggered updates, so we will need to send
    // ours out if and only if there has been an update.

    // Your code here...

    // Spin through our table and compare costs.  We also need to note, when we first
    // discover a route to a new destination, so we can enable the sending application
    // for that route.

    // for costs use, linkinfo[link].costperframe
    // to enable an application use: CNET_enable_application(...)

    // And, your code here...
    int change = 0;
    NL_PACKET* p = (NL_PACKET*)(packet);
    int* table = (int*)p->msg;
    int i;

    for (i = 0; i < rtable_size; i++) {
        int cost = table[i] + linkinfo[arrived_on].costperframe;
        if (rtable[i][arrived_on] > cost) {
            rtable[i][arrived_on] = cost;
            change = 1;
            CNET_enable_application(p->src);
        }
    }

    if (change) {
        send_table();
    }
}

/*  up_to_network() IS CALLED FROM THE DATA LINK LAYER (BELOW) TO ACCEPT
    A PACKET FOR THIS NODE, OR TO RE-ROUTE IT TO THE INTENDED DESTINATION.
    */

int up_to_network(char *packet, size_t length, int arrived_on)
{
    NL_PACKET *p = (NL_PACKET *)packet;

    p->hopcount++; /* took 1 hop to get here */
    switch (p->kind) {
    case NL_ROUTE: {

        // Have received a routing table, need to check against ours and update
        // if needed.  If updated, trigger updates to others.  Doesn't matter who
        // it is addressed to, because the sender doesn't know.

        update_routing_table(packet, p->length, arrived_on);
        break;
    }
    case NL_DATA: {
        // It is a data packet, if it is for us, & seq# is correct consume it.

        if(p->dest == nodeinfo.address) {   /* This one is for us. */
            if(p->seqno == NL_packetexpected(p->src)) {
                CnetAddr tmpaddr;

                CHECK(CNET_write_application(p->msg, &p->length));
                inc_NL_packetexpected(p->src);
                /* send back an ACK, change type and other fields though */

                tmpaddr = p->src;   /* swap src and dest addresses */
                p->src  = p->dest;
                p->dest = tmpaddr;

                p->kind     = NL_ACK;
                p->hopcount = 0;
                p->length   = 0;
                send_packet(packet, PACKET_HEADER_SIZE); // send ACK
            } else {
                fprintf(stderr,"\n\n Out of sequence data packet!! on %s\n", nodeinfo.nodename);
                fprintf(stderr,"Expected %i got %i\n", NL_packetexpected(p->src), p->seqno);
                exit(-1);
            }
        } else {  /* OTHERWISE, THIS PACKET IS FOR SOMEONE ELSE */
            if(p->hopcount < MAXHOPS)
                send_packet(packet, length);
        }
        break;
    }
    case NL_ACK : {
        if(p->dest == nodeinfo.address) {   /* This one is for us. */
            if(p->seqno == NL_ackexpected(p->src)) {
                inc_NL_ackexpected(p->src);
                CHECK(CNET_enable_application(p->src));
            } else {
                fprintf(stderr,"\n\n out of sequence ACK!! on %s\n", nodeinfo.nodename);
                exit(-1);
            }
        } else {  /* OTHERWISE, THIS PACKET IS FOR SOMEONE ELSE */
            if(p->hopcount < MAXHOPS)
                send_packet(packet, length);
        }
        break;
    }
    }
    /* silently drop */;

    return(0);
}

void show_table()
{
    int i,j;

    for(i = 0; i < rtable_size; i++) {
        printf("\n Destination %d || ",i);
        for(j = 0; j <= nodeinfo.nlinks; j++) {
            printf(" %d | ",rtable[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}


int init_routing()
{
    // OK, here we set up the routing table etc.   We know how many nodes there are
    // and we'll assume that the address and the nodenumber are the same.  This is
    // to create the simplest possible version of this lab.

    int link, node;

    rtable_size = NNODES;
    rtable = (int**)malloc(sizeof(int*)*rtable_size+1);

    for(node = 0; node < NNODES; node++) {
        // Create and allocate a single row, with one entry per physical link.

        rtable[node] = (int*)malloc(sizeof(int)*nodeinfo.nlinks+1);

        // Set all routes to 'unknown'

        for(link = 0; link <= nodeinfo.nlinks; link++)
            rtable[node][link] = INF;
    }

    // Now,update the table, to show we can get to ourselves!

    rtable[nodeinfo.nodenumber][0] = 0;

    // Set a UI handler to inspect this node's table.

    CHECK(CNET_set_debug_string(EV_DEBUG1, "Route Table"));
    CHECK(CNET_set_handler(EV_DEBUG1, show_table, 0));

    // Now, rather than start sending stuff, when other nodes may not have
    // booted yet, we'll set a timer.  Indeed we want the timer anyway.

    CHECK(CNET_set_handler(EV_TIMER1, send_table, 0));
    CNET_start_timer(EV_TIMER1, NL_TIMER_INIT, 0);

    // We shouldn't need to do this, but....

    CHECK(CNET_disable_application(ALLNODES));

    return 0;
}

/* ----------------------------------------------------------------------- */

EVENT_HANDLER(reboot_node)
{
    // for NWEN243 we're going to assume we know how many nodes there are
    // in our network.  This isn't realistic - but this is intended to be
    // the simplest example.

    if(NNODES < 2) { // set to 0 if the cmd line isn't set
        fprintf(stderr,"Please set command line parameter -N\n");
        exit(-1);
    }

    reboot_DLL();
    reboot_NL_table();

    CHECK(CNET_set_handler(EV_APPLICATIONREADY, down_to_network, 0));

    init_routing();  // Deliberately last!
}
