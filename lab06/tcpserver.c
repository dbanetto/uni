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

int sock;

void eatZombies(int n)
{
    // This function removes the zombie process state left
    // after the child exits. Learn about this in NWEN 301!

    wait3(NULL,WNOHANG,NULL); // Nom Nom
}

void cleanup() {
    if (sock > 0) {
        close(sock);
    }
}

void offset(char* str, int len, int offset) {
    int i = 0;
    for (;i < len; i++) {
        str[i] = (str[i] + offset) % 127;
    }
}

int main(int argc, char *argv[])
{
    sock = -1;
    struct sockaddr_in server;

    // for forking, and cleaning up zombie child state afterwards
    // You must not change this code.

    pid_t id;
    signal(SIGCHLD, &eatZombies);
    atexit(&cleanup); // Insure the

    // OK, NWEN 243 code starts here.

    // Create a socket (see Lab 2) - it is exactly the same for a server!
    // YOUR CODE HERE
    if(argc != 2){
        printf("usage:\ntcpserver port\n\n");
        return(-1);
    }

    // Next, create the socket addressing structure
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = htons(atoi(argv[1])); // this time 1st arg is port#

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Failed to open socket");
        exit(1);
    }

    // Next you need to BIND your socket.
    // YOUR CODE HERE
    if (bind(sock, (struct sockaddr*)&server, sizeof(server)) < 0) {
        perror("Failed to bind socket");
        close(sock);
        exit(1);
    }

    // Right, now we are in the main server loop.
    // YOUR CODE HERE

    // Start by listening
    if (listen(sock, BACKLOG) < 0) {
        perror("Failed to listen");
        close(sock);
        exit(1);
    }

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
            // parent
            close(client_sock); //close socket on parent thread
            continue;
        }
        // child
        // when id == 0, this is the child and needs to do the work for the server.

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

        offset(buffer, result, 10);
        if (write(client_sock, buffer, result) < 0) {
            perror("Failed to write from client");
            close(client_sock);
            exit(1);
        }

        // Your code here for the child process, that will read from the connection socket, process the data
        // write back to the socket, and then close and finally call exit(0) when done.
        fprintf(stderr,"%i: Disconnected\n", client.sin_port);
        free(buffer);
        shutdown(client_sock, SHUT_RDWR);
        close(client_sock);
        exit(0);

        // Note:  make sure the parent process (id > 0) does not execute this code, but immediately loops back
        // around (via the while) to get another connection request.

    }
}

