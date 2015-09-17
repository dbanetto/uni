#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define BUFFER_SIZE 1024
#define BACKLOG 10

// have to keep sock fd global so it can be closed later
int sock;

void eatZombies(int n)
{
    // This function removes the zombie process state left
    // after the child exits. Learn about this in NWEN 301!

    wait3(NULL,WNOHANG,NULL); // Nom Nom
}

void cleanup() {
    // clean up the socket if it has been inited successfully
    if (sock > 0) {
        close(sock);
    }
}

int main(int argc, char *argv[])
{
    sock = 0;
    struct sockaddr_in server;

    // for forking, and cleaning up zombie child state afterwards
    // You must not change this code.

    pid_t id;
    signal(SIGCHLD, &eatZombies);
    atexit(&cleanup); // Insure the socket will be cleaned up

    // OK, NWEN 243 code starts here.

    // Create a socket (see Lab 2) - it is exactly the same for a server!
    // YOUR CODE HERE
    if(argc != 2) {
        printf("usage:\ntcpserver port\n\n");
        return(-1);
    }

    // Next, create the socket addressing structure
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = htons(atoi(argv[1])); // this time 1st arg is port#

    // make socket
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Failed to open socket");
        exit(1);
    }

    // bind socket
    if (bind(sock, (struct sockaddr*)&server, sizeof(server)) < 0) {
        perror("Failed to bind socket");
        close(sock);
        exit(1);
    }

    // Start by listening
    if (listen(sock, BACKLOG) < 0) {
        perror("Failed to listen");
        close(sock);
        exit(1);
    }

    // main server loop
    while(1) {

        // you need to accept the connection request
        struct sockaddr_in client;
        int clientlen = sizeof(client);

        int client_sock = accept(sock,
                (struct sockaddr *)&client,
                (socklen_t *) &clientlen);

        if (client_sock < 0) {
            perror("Failed to accept client");
            continue;
        }
        fprintf(stderr,"Client connected on port %i\n", client.sin_port);

        // the next call makes a new child process that will actually handle the client.
        id = fork();
        // when id < 0, we had an error.
        if (id < 0) {
            perror("Failure in fork");
            exit(1);
        } else if (id > 0) {
            // when if > 0, this is the parent, and it should just loop around,
            close(client_sock); // close socket on parent so it can be fulled closed in the child
            continue;
        }
        // now in child fork()
        // deal with client
        char* buffer = NULL;
        buffer = (char*)(calloc(BUFFER_SIZE, sizeof(char)));
        if (buffer == NULL) {
            perror("Failed to allocate buffer");
            close(client_sock);
            exit(1);
        }

        int result = read(client_sock, buffer, BUFFER_SIZE);
        if (result < 0) {
            perror("Failed to read from client");
            close(client_sock);
            exit(1);
        }
        fprintf(stderr,"%i: %s\n", client.sin_port, buffer);

        if (write(client_sock, buffer, result) < 0) {
            perror("Failed to write from client");
            close(client_sock);
            exit(1);
        }

        fprintf(stderr,"%i: Disconnected\n", client.sin_port);

        free(buffer);
        // graceful closure of the connection
        shutdown(client_sock, SHUT_RDWR);
        close(client_sock);

        exit(0);
    }
}

