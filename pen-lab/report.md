% Penetrations Lab
% David Barnett - 300313764

# Part 1 - Penetration Testing Tools

<!--
 intro to:
  * Equipment interconnection diagram
  * Summart of facilities of 3 pen testing tools
      + Zenmap
      + FileZilla
      + Nessus
  * Note similarities & differences
-->

## Network

![Network diagram of Part 1 setup](./network_diagram.png)

Figure 1 shows the structure of the network
used in the first part of the lab.
There are three components used in this network (from left to right),

 * The Bank server,
 * Firewall, and
 * Penetration Tester's machine

The diagram shows that the Penetrator's machine has a configured
firewall between it and the target, bank server.

The bank server was a host to web service and a database.
This was to provide a banking service to clients.

## Tools used

There were three tools used during the first part of the lab.
These were:

 * Zenmap,
 * FileZilla, and
 * Nessus

Each of these tools have different uses throughout the penetration testing.

### Zenmap (nmap)

`Zenmap` is a GUI front end for a network exploration tool `nmap`.
`nmap` is an open source command line tool that is used during the discovery phase
of the penetration testing of the banking system.
This tool was designed to be used for this purpose
The tool provides features that can discover a range of information about
a network and the hosts on it [@pen-test].
One of the features it can discover and map the topology of a network by
pinging hosts and using trace routes to build up the map.
Another feature allows `nmap` to scan the ports of a target host
machine for open ports and relates then to services.
It goes one step further by attempting to finger print the open services
but trying to identify what software is running the service and what version, e.g.
testing port 80 and finds out it is an Apache web server running version
`2.4.27` [@nmap].
`nmap` by itself is very feature rich but lacks the accessibility and
requires the user to parse and understand the output of the commands to
be useful.
`Zenmap` solves this issue by providing a GUI over top of
these features and makes then even more accessible.
This has been achieved by parsing the output of a normal `nmap` command and
displaying it with additional hints, such as colouring and in the case of
the network topology even a map.
With these features the `Zenmap` tool is an accessible reconnaissance tool.

<!--
* recon for other tools to then use
* gives hints at what services could be exploitable
* list the ports of interest that were found
* tried anon access to some services
* finger printed services, e.g. IIS host & version
-->

### FileZilla

`FileZilla` is a GUI FTP client that is used to interact with FTP service.
This is an open source tool that in the penetration lab was used to as
an attack tool to attempt to test and exploit a potential weakness on
the bank server [@pen-test].
This tool was not designed to be used as a part of penetration testing but
is useful as a consequence of FTP being exploitable [@ftpsec].
In this lab it was found via `Zenmap` that the target Bank machine allows
anonymous access. This was tested with `FileZilla` by directly connecting
to the machine via FTP. From experimenting with the connection reading
the files in the FTP directory was allowed, however writing to the server
was not.

Compared to `Zennmap` and `Nessus`the information gained from using this tool is limited.
Using `FileZilla` in this penetration testing allows for testing the FTP service
as a well behaved user.
However, `Nessus` informs the tester of what is possible as a
malicious user.
This shows that `FileZilla` is a good tool for testing for
misconfiguration of FTP services, such as anonymous write access,
but does not reveal deeper vulnerabilities such as the potential of
bounce attacks from a malicious client [@ftpsec].

<!--
 * Used to prod into open anon FTP
 * could not write to disk
 * could read listing
 *
-->

### Nessus

`Nessus` is a commercial tool to scan and report vulnerabilities
in system.
`Nessus` scans for a range of possible vulnerabilities  from checking
for default passwords, misconfiguration, known exploits and more.
This tool is used during the discovery phase to identify common
vulnerabilities in a system [@pen-test][@nessus].
In the lab this tool was used to identify possible vulnerabilities
of the system, a complete scan showed over 40 potential exploits.

`Nessus` shares common functionality with `Zenmap`.
Both of these tools would be used during the discovery phase
but with different aims [@pen-test].
Where `Zenmap` is more focused on identifying open ports and fingerprinting
services `Nessus` expands on this by testing for known vulnerabilities
either by exploits or misconfiguration.
Both of these tools provides reporting back to the user to understand
what has been discovered of the network or a single host.
However, the main draw back of `Nessus` against `Zenmap` is that
it is a commercial product as opposed to an open-source project.

 <!--
 *
 -->

### Dotdotpwn

`dotdotpwn` is a vulnerabilities testing tool that uses fuzzing to discover
directory traversal exploits.
The tool supports fuzzing over HTTP and FTP urls.
It tests for possible urls that would read from an arbitrary path on
the target machine.
For example the HTTP server behind `example.com` might allow or be poorly
configured that a malformed request such as `http://example.com/../server.crt`
would send the private key for the SSL of the server from the parent directory
on the server.
This is a major leakage of private information that could then lend to
handing over passwords stored on the server.
This tool is useful in the discovery phase of a penetration test as 
it givens additional information on possible attack vectors [@pen-test].

Compared to the other tools used in the lab `dotdotpwn` is very domain 
specific to malformed URLs.
Where `Zenmap` is useful for finding more general information about a
target host and some additional fingerprinting if it can.
`Nessus` is similar in regard of find particular exploits of the host but
differ in the range of exploits they seek to detect.
As opposed to all of the other tools `dotdotpwn` would be a good
addition to a set of security integration tests to ensure
future development of the system do not introduce this kind of exploit.


![Screen shot of Kali Linux running dotdotpwn on local web server](./img/dotdotpwn_1.png)

![Additional output of dotdotpwn test on local web server](./img/dotdotpwn_2.png)

\pagebreak

# Part 2 - Intrusion Detection

<!--
 * describe use of SNORT
 * explain how the CLI version functions
 * discuss monitoring & reporting facilities of SNORBY
-->

## SNORT

`SNORT` is an open source network intrusion detection system (NIDS).
The objective of `SNORT` is to monitor and warn of detected
attacks or probes from potential attacks.
In this lab `SNORT` was used by the monitoring laptop in the
Bank.
The NIDS function is the main feature of `SNORT` but it also
exposes the key features that enable it, packet sniffing and
packet logging [@snort].
To achieve its NIDS goal `SNORT` sniffs all of the packets
that are received by the host and applies a user defined rule
set to identify the traffic, either as normal or as a possible
attack.
The use of user defined rule sets are a major feature of
`SNORT` and is what enables it to be a great tool to
monitor a system.


## SNORBY

`SNORBY` is an open source web front-end to multiple
intrusion detection system (IDS) [@snorby].
`SNORBY` consumes the logs of IDS and presents them
with a user friendly interface that includes graphs
and summary dashboard.
In the lab this tool was used to easily digest the
information that `SNORT` found via monitoring the network.
From `SNORBY`  providing an easy to use interface of
the IDS data it allows for system administrators to be able
to react faster to possible attacks as it is clearer when
graphs spike then reading and digesting a wall of text from
tools like `SNORT`.
However, this tool looks to be abandoned, as there has been
no recent developments on their public repository and
their domain ( snorby.org ) has lapsed and now serves
domain registration advertising.

`SNORT` and `SNORBY` are not competing tools but are
supplementary to each other.
This is because `SNORT` is designed to be a configurable NIDS
that outputs a large log of what it has detected during its
monitoring of the network.
`SNORBY` then supplements this by consuming the log of what
have been monitored and presents this to the administrators
in an easy to digest format of graphs and summary statistics.

## Fail2Ban

`Fail2Ban` is another open-source intrusion detection system
that focuses on preventing brute force attacks [@fail2ban].
This is achieved by monitoring logs of applications, such
as apache web server, sshd and more to identify remote
hosts that are attempting to brute force passwords or exploits.
To prevent further attempts the IP address is then added to
a blacklist on the host firewall to drop all requests from
untrusted user.

`Fail2Ban` has the same overall goal as `SNORT` and `SNORBY` of
detecting possible intrusions, but the method to achieve this
is vastly different.
For `Fail2Ban` it achieves this goal by detecting
a possible attacker and then blocking them via the firewall.
While, `SNORT` monitors for possible attack that the user
has defined and `SNORBY` displays the findings of the monitoring.


## Identified Risks

During the lab a range of potential security risks
were identified. Some of these risks are exploitable
from a remote host.
These include:

 * the anonymous access to the FTP service,
 * MySQL server port open across the firewall,
 * SMB/CIFS ports open across the firewall, and
 * MS-RPC port open across the firewall.

### Anonymous FTP Access

The anonymous access to the FTP service opens the
risk of being used in a port bounce attack [@ftpsec].
This attack abuses the FTP protocol to send a file to
a target machine.
The Bank can mitigate this issue in multiple ways.
If the FTP service is designed to be local to within the
business the FTP service itself could be configured to only
accept connections from inside the same network or subnet.
Another approach would be to configure the firewall to reject
all attempts for external hosts connecting to the FTP service.
These configuration can be managed by the system administrators
by documenting the changes and the reasons why, or implement
the changes with a configuration-as-code package such as Puppet.
If the FTP service is suppose to be used publicly,
the FTP service could be patched or choose a
server implementation that supports the prevention of
sending files to hosts other than the connected host.
This can then be maintained by the system administrators
via documentation of the reasons why a particular patch
was applied or server was chosen.
The use of a patch is less maintainable as it requires
additional effort to update the service as it would require
more testing after it has been applied.

### MySQL

Another potential risk is exposing the MySQL port over the
firewall.
This allows for the risk of an attacker brute forcing the
password or acquiring it somehow and having access to the
databases.
There are some methods to mitigate this risk.
One would be to configure the MySQL service to only accept
connections from the local network or the current subnet.
This would prevent direct access from a remote attacker.
Another method would be to block external connections
to MySQL by configuring the firewall to block it.
These can be maintained by the system administrators through
documenting the configuration used for the services
and by implementing and applying them via tools such as
Puppet or Chef to ensure they are set.

However, both of these options assume that the Bank does
not allow for external uses of the MySQL database.
If the Bank did want this a possible mitigation would be
to use a tool like `Fail2Ban` to prevent brute force attacks
on default users like `mysql` or leaked user names.
This would mitigate brute force attacks but would not
help to prevent a user with stolen credentials outside of
the network accessing the database.
A possible solution would be to configure the service to use
a two factor login for MySQL.
The maintenance for this solution is a lot heavier than
blocking external users, as it requires the configuration and
maintained of an additional local service.

### SMB / CIFS Ports open

### MS-RPC Ports open

<!--
 * Identify remote services in operation
   + should be 4 of them
 * recommend secure methods of operation
   + either recommend to shutdown or,
   + hot to secure them
 * explain how the systems staff will carried out:
   + maintenance
   + patches
   + updates
   + new developments are installed for clients
   + how Cloud Services will operate in practice
-->

\pagebreak

# Part 3 - Interception of Encrypted Traffic

<!--
  Intercept encrypted traffic
  Provide summary of:
   * how the intersception works
   * how it is  possible to extract personal data from HTTPS
  (NOTE: ONLY IT ENGR side, not legal/ethics of it)
-->

\pagebreak

# Bibliography
