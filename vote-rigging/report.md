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

The purpose of the hack is to discredit the confidentiality of the election ballot.
This is achieved by removing the voter anonymity by exporting the ballot and ensuring
the ballot order can be restored.
The result of this hack allows the paymasters to associate the full ballot with an individual 
based on the order that they voted.


# Trigger
<!--
Describe the conditions for enabling or activating the backdoor. Length is going to be modification-specific. For example, you may need to provide command line switches and explain what each one does.

You can assume that either your paymasters can bribe people running the machines to make small changes locally, run programs with extra command line options or simply send one of their agents to each of the locations while pretending to be a legitimate voter.
-->

# Design & Implementation
<!--
Identify the key parts of the code base that have been changed and add a commentary.

I would expect maybe a couple of paragraphs giving an overview of code structure, indication of how many lines have been changed and a discussion of the general approach that connects the purpose of the backdoor with the changes to the code (for example, if the desired outcome is for Party Z to always have the majority votes you will would need to explain how your changes modify the results).

You should include a walkthrough of the key parts of the code with differences from the original and modified code highlighted. 

-->

# Self-Assessment
<!--
Briefly self-assess how well the backdoor is hidden (in terms of design and implementation) from the voter using the system and from a potential auditor.

Briefly self-assess whether your implementation is one based upon the article about Hack-a-Vote or somehow extends it and/or goes beyond what was described.

HINT. Consider the whole system when hiding your hack.
-->
