% David Barnett
% NWEN243 300313764

# Question 1

It takes some time to detect the other party is not reachable either
through the other party is not responding to keep-alive or just not having
heard, for example packets not acknowledged, for longer than the socket time-out
time.

<!-- http://tldp.org/HOWTO/TCP-Keepalive-HOWTO/overview.html -->

# Question 2

No. TCP decides when is the best time to send the data through congestion and flow control
to best maximise the network bandwidth to the other party and minimize the network congestion.

<!-- http://www.unixguide.net/network/socketfaq/2.11.shtml -->

# Question 3

Out-of-band data in socket communications is a block of data which is labeled as urgent.
The urgency is observed by the application layer only and interrupts the current data
stream with the urgent bulletin until the data is received then normal programming continues.

# Question 4

To send a series of messages reliably in order with sockets over the internet requires a method
to keep track of the order of the sent packets and also to acknowledge reliably to the other party.
A method to keep track of the order of the packets is to use a sliding window approach so the receiver
is expecting to receive a packet that is identified to be within the range. The receiver would also acknowledges
the packets received in order to the other party. A method to implement reliability would be also using the acknowledge
packets but re-sending packets after they are not acknowledged after a period of time.

# Question 5

The SPARC system could get the correct integer from the socket given that there is a protocol in place to send
the integer through the socket. Otherwise the integer will not be correct as due to the network byte order the
most significant byte will represent a higher value than SPARC can represent. However the sign of the integer will be
correct but the actual value will not.
