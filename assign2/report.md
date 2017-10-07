% Assignment 2 - NWEN405
% David Barnett (300313764)

# Revision Questions

## A)

A host-based intrusion detection (IDS) system is specialised software layer that detects
intrusions via a range of methods such as anomaly detection of local resource usage,
signature detection of known attack patterns and heuristics of the system.
A network-based intrusion detection system uses similar techniques as host-based IDS to
detect issues on the network. 
The main difference between the two systems is that a network based IDS needs
to handle all traffic going through it so that finding patterns between anomalies 
is a computational and data heavy task. Where a host-based IDS focuses on the single machine
it is monitoring to discover potential intrusions.

>>>> FIX ME <<<<

## B)

A false positive is when a heuristic based test says it has passed but when
tested again with a thorougher test that is certain in its result shows it to be
fail.
An example of this for a IDS such as a network-based one would be wrongfully detecting
a new sequence of packets to be an attack where in fact it was a new protocol developed
in-house.
A false negative  is similar to a false positive but the heuristic wrongfully
decides that it fails when the certain test says it should pass.
An example of this for a IDS such as a network-based one would be wrongfully detecting
that an attack is instead normal network usage.

## C)

 * Statistical - the detection system monitors for statistical anomalies such
  as a high disk usage at a time that is known to be a historic low.
  Though this method can create false-positives from changes in behaviour, such
  as a sysadmin realising it is a good idea to do daily backups instead of weekly.

 * Model based detection - is based off a defined model of a working system.
 Events in the system moves the model to new states where it has "acceptable" transitions.
 If actions are taken that is not predicted by the model it is considered an
 anomaly and reported. This method is heavy as ability to create a well defined model
 is difficult.

 * Machine learning based detection - an advancement on the statistical monitoring
  to use machine learning techniques to observe more patterns. 
  Like the model based detection a ML detection can observe when a system diverges from
  the known patterns to flag as an anomaly.

## D)

 1. The firewall must be placed where all traffic (intended to be filtered) must pass through it
 2. Only authorized traffic, that is defined by the firewall's policy, is allowed to pass.
 3. The firewall cannot be subverted

## E)

A packet filtering firewall looks at a single packet and match it against simple rules,
such as no traffic to IP range `192.168.0.1/24`.
These policies are simple pattern matching on the bytes of the packet.
This allows these types of firewalls to be implemented as hardware to be fast. 
A stateful firewall buffers or remembers multiple packets to figure out connections
between machines that pass the firewall.
The difference between these types of firewalls are that a packet filtering firewall
only considers a single packet to apply its policies to while a stateful firewall
considers the connections, packets over time, in its policies.

## F)

A intrusion  prevention system differs from a firewall.
This is due to IPS can run a full check up to application level to 
ensure properties such that HTTP request are valid and not known attack
patterns. This allows an IPS to gain a context of the packet to allow for
better detection and prevention.
While a firewall will enforce the packet policy deployed by the
administrators.

## G)

The two key elements to implement a buffer overflow attack is the ability
to overwrite some additional memory than the buffer was allocated for so that
the additional data could then spill and corrupt other structures in memory.
The second element is the ability to move the execution of the program to the
attackers code, such as overwriting a function pointer of some kind to the 
attacking code.

## H)

 * Flagging different pages of memory as data or executable, this separates the buffer from code that could be potentially overwritten by the overflow.
 * Write protect executable memory, assuming that self-mutating code is undesirable
   the ability to mutate any memory flagged as executable will prevent potential
   overwrites into that memory space.
 * Canary objects on stack frames to detect attempts to overwrite the stack pointer to
   the attacker's function

## I)

Software quality and reliability refers to well a code base is formatted and
how well it handles errors states.
This has the intention to reduce the attack surface from unmaintainable code
and poorly handled error states.
Where software security is about how the code is structured to prevent or
minimize the impact of a potential bug or exploit.
This has the intent to limit the damage cause by any exploit that may or may not exit
by implementing security models or enforcing separation of concerns.

## J)

An command injection is the act of executing a system command, `dir` for DOS
machines (Windows) or `ls` on UNIX machines, via an input field due to formatting
it in a particular manner or exploiting a bug in the software.
An SQL injection is similar to a command inject but instead of targeting a
system call it targets commands being sent to the SQL server as part of a
request.

## K)

Input fuzzing is when a user input field, such as a free text field in a GUI or environment variables, is 
tested against a large range of random strings that include either garbage data or
just plain wrong encoding to test how well the program can handle the input.
One field this is used in is to test compilers or serializing frameworks to ensure
that the fuzzed inputed does not result in an unexpected error or success.
This should be used for any user-input field that allows free-form text.

## L)

A security issue that can arise form the careless use of environment variables
are underlying assumptions should be broken.
For example the `PATH` variable maybe overwritten or shimmed it look at
`/tmp/bad/bin` before the correct system binaries or common commands such as
`cd` or more.
By doing so an attacker can run their own code at the permission level of the script
to launch their attack.
This could have outcome of an attacker taken over an entire user, for example `root`
or `httpd`.

# Practical Component (Squid Proxy)

## Part 1

## Part 2

## Part 3

## Part 4

## Part 5
