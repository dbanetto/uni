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
vulnerabilities in a system [@pen-test].
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

## Additional Tools used



# Part 2 - Intrusion Detection

<!--
 * describe use of SNORT
 * explain how the CLI version functions
 * discuss monitoring & reporting facilities of SNORBY
-->

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
