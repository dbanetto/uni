/*
 * "Hack-a-vote", a Direct-Recording Electronic (DRE) voting machine
 * software implementation.
 * 
 * Copyright 2003, Rice University. All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * - Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * - Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the
 * distribution.
 * 
 * - Neither the name of Rice University (RICE) nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * 
 * This software is provided by RICE and the contributors on an "as
 * is" basis, without any representations or warranties of any kind,
 * express or implied including, but not limited to, representations
 * or warranties of non-infringement, merchantability or fitness for a
 * particular purpose. In no event shall RICE or contributors be
 * liable for any direct, indirect, incidental, special, exemplary, or
 * consequential damages (including, but not limited to, procurement
 * of substitute goods or services; loss of use, data, or profits; or
 * business interruption) however caused and on any theory of
 * liability, whether in contract, strict liability, or tort
 * (including negligence or otherwise) arising in any way out of the
 * use of this software, even if advised of the possibility of such
 * damage.
 */

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

import java.awt.Dimension;
import java.awt.event.*;
import java.io.*;
import java.net.*;

/**
 * BallotControl<p>
 *
 * The main voting machine control. Run me to launch an election.
 *
 * @author Dave Price <dwp@alumni.rice.edu>
 * @edited Brian Armstrong <barmstrong@gmail.com>
 */
public class BallotControl {

	/**
	 * List of ballotPanels
	 */
	static List ballotPanels;

	/**
	 * The voting machine GUI
	 */
	static BallotGUI bg;

	/**
	 * Array of election names.
	 */
	static String[] ballots;

	/**
	 * The ballot confirmation panel.
	 */
	static BallotConfirmPanel confirmPanel;

	/**
	 * The ballot output directory File object
	 */
	static File dirFile;

	/**
	 * Read the ballot from this file.
	 */
	static String ballotFile;

	/**
	 * Output filled-in ballots into this directory.
	 */
	static String ballotOutDir;

	/**
	 * Whether we're in test mode
	 */
	static boolean selfTestMode = false;

	/**
	 * Directory of test inputs.
	 */
	static String testInputDirectory = null;

	/**
	 * When we are testing ourselves, how many votes to cast
	 */
	static int numTestVotes = 30;

	/**
	 * Whether to require a PIN to authenticate
	 */
	static boolean PIN_AUTHENTICATION = true;

	/**
	 * The name of the host that's doing authentication for us
	 */
	static String authHost = "localhost";

	/**
	 * The port to connect to to authenticate.
	 */
	static int authPort = Console.CONSOLE_PORT;

	/**
	 * Return code from console if PIN accepted
	 */
	static String PIN_ACCEPT_PREFIX = "300";

	/**
	 * Return code from console if PIN rejected
	 */
	static String PIN_DENY_PREFIX = "400";

	/**
	 * The PIN query panel
	 */
	static PINQueryPanel pq;

	/**
	 * The main GUI frame.  Contains all other GUI elements.
	 */
	static JFrame outsideFrame;

	/**
	 * The master list we're keeping. Saved in memory until
	 * administration says to write it out to file
	 */
	static List castBallots;

	/**
	 * Main method.
	 */
	public static void main(String[] args) {

		/* COMMAND LINE OPTIONS */

		// What file we read the ballot from
		ballotFile = "form";
		ballotOutDir = "ballotbox";

		/* Go through the args for arguments */
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-formfile")) {
				if ((i + 1) < args.length) {
					i++;
					ballotFile = args[i];
				} else
					errorExit("-formfile requires an argument");
			} else if (args[i].equals("-ballotdir")) {
				if ((i + 1) < args.length) {
					i++;
					ballotOutDir = args[i];
				} else
					errorExit("-ballotdir requires an argument");
			} else if (args[i].equals("-guidedtest")) {
				if ((i + 1) < args.length) {
					i++;
					selfTestMode = true;
					testInputDirectory = args[i];
				} else
					errorExit("-guidedtest requires a directory argument");
			} else if (args[i].equals("-randomtest")) {
				selfTestMode = true;

			} else if (args[i].equals("-h")) {
				printHelp();
				System.exit(0);
			}
			// more if clauses go here as we add features
			else
				errorExit("unknown command line option \"" + args[i] + "\"");
		}

		dirFile = new File(ballotOutDir);
		if (!dirFile.exists()) {
			System.out.println("Ballot dir doesn't exist");
			dirFile.mkdir();
		}

		File f = new File(ballotFile);
		if (!f.canRead()) {
			errorExit("Can't read ballot form file " + ballotFile);
		}

		ballotPanels = new LinkedList();
		castBallots = new LinkedList();

		/* Read in the ballot file */
		IniFile bFile;
		bFile = new IniFile(ballotFile);
		ballots = bFile.getSubjects();
		for (int i = 0; i < ballots.length; i++) {
			LinkedList candidates = new LinkedList();
			String[] names = bFile.getVariables(ballots[i]);
			for (int j = 0; j < names.length; j++) {
				Candidate c = new Candidate(names[j], bFile.getValue(
						ballots[i], names[j]));
				candidates.add(c);
			}
			BallotPanel b = new BallotPanel(ballots[i], candidates);
			ballotPanels.add(b);
		}

		/* Throw up a frame with the results */
		outsideFrame = new JFrame(BallotGUI.NAME);
		outsideFrame.setSize(new Dimension(600, 700));
		bg = new BallotGUI((BallotPanel[]) ballotPanels
				.toArray(new BallotPanel[0]));
		outsideFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		outsideFrame.getContentPane().add(bg);
		outsideFrame.setVisible(true);
		//outsideFrame.pack();

		/* Set up listeners and such */

		// Listen for candidates being selected (the dummy noneButton
		// changes state)
		ListIterator it = ballotPanels.listIterator();
		while (it.hasNext()) {
			JToggleButton thisNone = ((BallotPanel) it.next()).noneButton;
			thisNone.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					boolean isComplete = true;
					ListIterator it2 = ballotPanels.listIterator();
					while (it2.hasNext()) {
						if (((BallotPanel) it2.next()).selectedCandidate() == null) {
							isComplete = false;
							break;
						}
					}
					if (isComplete) {
						bg.commitVote.setEnabled(true);
					}
				}
			});
		}

		// Add an action listener for the vote commit button
		bg.commitVote.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent a) {
				// Disable all user buttons
				bg.nextBallotLine.setEnabled(false);
				bg.prevBallotLine.setEnabled(false);
				bg.commitVote.setEnabled(false);

				// Build the Hashtable mapping election names
				// to candidates voted for.
				Hashtable votes = new Hashtable();
				ListIterator it = ballotPanels.listIterator();
				while (it.hasNext()) {
					String electionName;
					Candidate votedFor;

					BallotPanel panel = (BallotPanel) it.next();
					electionName = panel.title;
					votedFor = panel.selectedCandidate();
					votes.put(electionName, votedFor);
				}

				confirmPanel = new BallotConfirmPanel(votes, ballots);
				confirmPanel.yesButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent a) {
						saveVotes(confirmPanel.votes);
						BallotControl.resetBallot();
					}
				});
				confirmPanel.noButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent a) {
						BallotControl.resetBallot();
					}
				});

				// Add the confirmation panel to the appropriate place in the GUI
				bg.ballotPanelHolder.removeAll();
				bg.ballotPanelHolder.add(confirmPanel);
				bg.confirmPanel = confirmPanel;
				bg.ballotPanelHolder.validate();
				bg.ballotPanelHolder.repaint();
			}
		});

		// Add a listener to the administer button
		bg.administer.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent a) {
				endElection();
			}
		});

		// If we're not self-testing, and we're authenticating via PIN, require a PIN here.
		if (PIN_AUTHENTICATION && !selfTestMode) {
			bg.setEnabled(false);
			requirePIN();
		} else { // Go ahead and let the user proceed
			bg.setEnabled(true);
		}

		// If we're in self-test mode, go ahead and have that test happen
		if (selfTestMode) {
			SelfTester st = new SelfTester(bg, testInputDirectory);
			if (testInputDirectory == null)
				st.test(numTestVotes);
			else
				st.test(0);
		}

	}

	/**
	 * Saves votes to the castBallots list.
	 *
	 * @param votes A hashtable mapping electionname to candidate
	 * voted for.
	 */
	static void saveVotes(Hashtable votes) {
		castBallots.add(votes);
	}

	/**
	 * Reset the ballot.
	 */
	static void resetBallot() {
		ListIterator it = ballotPanels.listIterator();
		while (it.hasNext()) {
			BallotPanel thisPanel = (BallotPanel) it.next();
			thisPanel.unselect();
		}

		bg.displayFirst();

		if (PIN_AUTHENTICATION && !selfTestMode) {
			bg.setEnabled(false);
			requirePIN();
		} else {
			bg.setEnabled(true);
		}

	}

	/**
	 * End the election, count the votes, and display the results.
	 */
	static void endElection() {

		// Array of hashtables with counts for each election. Initialize
		// each candidate's vote count to zero.
		Hashtable[] voteCounts = new Hashtable[ballots.length];
		for (int k = 0; k < voteCounts.length; k++)
			voteCounts[k] = new Hashtable();

		ListIterator it = ballotPanels.listIterator();
		int j = 0;
		while (it.hasNext()) {
			BallotPanel thisPanel = (BallotPanel) it.next();
			ListIterator i2 = thisPanel.ballotEntries.listIterator();
			while (i2.hasNext()) {
				Candidate thisCand = (Candidate) i2.next();
				voteCounts[j].put(thisCand, new Integer(0));
			}

			j++;
		}

		// Create a randomly ordered array to anonymize the ballots.
		Collections.shuffle(castBallots);
		ListIterator i3 = castBallots.listIterator();
		int bnum = 0;

		while (i3.hasNext()) {
			Hashtable thisVoter = (Hashtable) i3.next();
			bnum++;
			File outfile = new File(ballotOutDir, "ballot" + bnum + ".txt");
			PrintStream p = null;
			try {
				p = new PrintStream(new FileOutputStream(outfile));
			} catch (IOException e) {
				System.err.println("Can't write to ballot file: " + e);
				System.exit(-1);
			}

			for (int i = 0; i < ballots.length; i++) {

				String election = ballots[i];
				Candidate votedFor = (Candidate) thisVoter.get(election);

				// Record the vote by modifying the hashtable:
				// first, get the old vote count
				int n = ((Integer) voteCounts[i].get(votedFor)).intValue();
				// Set it to that plus one
				voteCounts[i].put(votedFor, new Integer(n + 1));

				// Output that vote to file
				p.println("[" + ballots[i] + "]");
				p.println("Candidate=" + votedFor.name);
				p.println("Party=" + votedFor.party);
				p.println();
			}

			p.close();

		}

		// Build up a textual representation of the counts
		StringBuffer sb = new StringBuffer("Vote totals\n");
		for (int i = 0; i < ballots.length; i++) {
			sb.append("\n");
			sb.append(ballots[i]);
			sb.append("\n");
			Enumeration candidates = voteCounts[i].keys();
			while (candidates.hasMoreElements()) {
				Candidate c = (Candidate) candidates.nextElement();
				sb.append(c.ballotString());
				sb.append(" received ");
				sb.append(((Integer) voteCounts[i].get(c)).intValue());
				sb.append(" votes.\n");
			}
		}

		// Show the results up in the panel.
		displayResults(sb.toString());
		bg.nextBallotLine.setEnabled(false);
		bg.prevBallotLine.setEnabled(false);
		bg.commitVote.setEnabled(false);
		bg.adminPanel.setEnabled(false);
	}

	/**
	 * Display the results of the election.
	 *
	 * @param voteResults String to be displayed, containing the
	 * results. 
	 */
	static void displayResults(String voteResults) {

		JTextArea displayText = new JTextArea(10, 30);
		displayText.setText(voteResults);
		displayText.setEditable(false);
		displayText.setVisible(true);

		JScrollPane scroll = new JScrollPane(displayText);
		scroll.setVisible(true);

		bg.ballotPanelHolder.removeAll();
		bg.ballotPanelHolder.add(scroll);
		bg.ballotPanelHolder.validate();
		bg.ballotPanelHolder.repaint();

		System.out.println(voteResults);
	}

	/**
	 * Command-line help
	 */
	static void printHelp() {
		System.out.println("Hack-a-vote help");
		System.out
				.println("-ballotdir {dir}\tSpecify directory for ballot box, default 'ballotbox'");
		System.out
				.println("-formfile {file}\tSpecify location of the ballot form file");
		System.out
				.println("-guidedtest {dir}\tEnter self-test mode guided by the given directory");
		System.out
				.println("-randomtest\tEnter self-test mode, casting random ballots");
		System.out.println("-h\t\tDisplay this help");
	}

	/**
	 * Exit on error.
	 *
	 * @param errstring Error string to display on exit.
	 */
	static void errorExit(String errstring) {
		System.err.println("Error: " + errstring);
		System.err.println("Exiting.");
		System.exit(-1);
	}

	/**
	 * Display a frame that does the job of authenticating via PIN
	 * number to the auth server.
	 */
	static void requirePIN() {
		pq = new PINQueryPanel();
		pq.setVisible(true);
		bg.displayPINQuery(pq);
		pq.pinField.requestFocus();
		//outsideFrame.getRootPane().setDefaultButton(pq.okbutton);

	}

	/**
	 * Check that the PIN encoded in the given query frame is correct,
	 * and enable voting if so.
	 *
	 * @param f The PINQueryPanel to get the PIN from. 
	 */
	static void checkPin(PINQueryPanel f) {
		int suppliedPIN;
		try {
			suppliedPIN = Integer.parseInt(f.pinField.getText());
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(outsideFrame,
				    "Invalid PIN number.",
				    "Error",
				    JOptionPane.ERROR_MESSAGE);
			return;
		}

		Socket s = null;
		try {
			s = new Socket(authHost, authPort);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(outsideFrame,
			    "Can't connect to admin console.",
			    "Error",
			    JOptionPane.ERROR_MESSAGE);
			return;
		}

		try {
			PrintStream sout = new PrintStream(s.getOutputStream());
			BufferedReader sin = new BufferedReader(new InputStreamReader(s
					.getInputStream()));
			sin.readLine();
			sin.readLine();
			sout.println(suppliedPIN);
			String validated = sin.readLine();
			if (validated.startsWith(PIN_ACCEPT_PREFIX)) {
				f.setVisible(false);
				bg.displayFirst();
				bg.setEnabled(true);
			}else{
				JOptionPane.showMessageDialog(outsideFrame,
					    "Invalid PIN number.",
					    "Error",
					    JOptionPane.ERROR_MESSAGE);
				return;
			}
		} catch (IOException e) {
			JOptionPane.showMessageDialog(outsideFrame,
				    "Error connecting to admin console.",
				    "Error",
				    JOptionPane.ERROR_MESSAGE);
			return;
		}

	}

}