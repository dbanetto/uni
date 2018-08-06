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

import javax.swing.*;
import java.util.*;
import java.io.*;

/**
 * SelfTester<p>
 *
 * The code that makes the voting machine test itself. When called upon,
 * this code surfs through the ballot and automatically casts random
 * votes, while remembering the number of votes cast. At the end,
 * it will present its own tally for comparison.
 *
 * @author Dave Price <dwp@alumni.rice.edu>
 */
public class SelfTester {

	/**
	 * Time (in ms) spent clicking on a given button.
	 */
	static final int CLICK_LENGTH = 50;

	/**
	 * Time (in ms) spent sleeping in between doing things in the
	 * tester. 
	 */
	static final int SLEEP_LENGTH = 50;

	/**
	 * max int
	 */
	static final int MAXINT = 2147483647;

	/**
	 * The voting machine GUI.
	 */
	BallotGUI bg;

	/**
	 * Random number generator.
	 */
	Random r;

	/**
	 * The directory where to find votes to cast
	 */
	String ballotFileDir = null;

	/**
	 * build a new testing harness that casts random votes
	 * @param bg The GUI to manipulate
	 */
	public SelfTester(BallotGUI bg) {
		this.bg = bg;
		r = new Random();
	}

	/**
	 * Build a new testing harness that casts the same votes as
	 * shown in the given ballot directory. Please note that
	 * the input directory should be different from the output directory
	 * set.
	 *
	 * @param bg The GUI to manipulate
	 * @param ballotFileDir The directory where to find votes to cast.
	 */
	public SelfTester(BallotGUI bg, String ballotFileDir) {
		this.ballotFileDir = ballotFileDir;
		this.bg = bg;
		r = new Random();
	}

	/**
	 * Perform the test, casting the specified number of randomly decided
	 * ballots.
	 * 
	 * @param howMany the number of ballots to cast.
	 */
	public void test(int howMany) {
		if (howMany == 0)
			howMany = MAXINT;
		for (int i = 0; i < howMany; i++) {
			bg.displayFirst();

			IniFile ballotFile = null;
			if (ballotFileDir != null) {
				File f = new File(ballotFileDir + "/ballot" + (i + 1) + ".txt");
				if (!f.exists()) {
					break;
				}
				ballotFile = new IniFile(ballotFileDir + "/ballot" + (i + 1)
						+ ".txt");
			}

			// Vote in each panel.
			for (int j = 0; j < bg.panels.length; j++) {

				AbstractButton candButton;

				Enumeration buttonE = bg.panels[j].group.getElements();
				LinkedList buttonL = new LinkedList();
				while (buttonE.hasMoreElements())
					buttonL.add(buttonE.nextElement());

				if (ballotFileDir == null) {
					// Pick any button that's not the dummy 'none'
					// button
					candButton = bg.panels[j].noneButton;
					while (candButton == bg.panels[j].noneButton) {
						candButton = (AbstractButton) buttonL.get(r
								.nextInt(buttonL.size()));
					}
				} else {
					String electionName = bg.panels[j].title;
					String testVoteCand = ballotFile.getValue(electionName,
							"Candidate");
					String testVoteParty = ballotFile.getValue(electionName,
							"Party");
					String expectedName = testVoteCand + " (" + testVoteParty
							+ ")";
					ListIterator it = buttonL.listIterator();
					AbstractButton clickMe = null;
					while (it.hasNext()) {
						AbstractButton thisButton = (AbstractButton) it.next();
						String buttonName = thisButton.getText();

						//DEBUG
						System.err.println("Comparing '" + buttonName + "' = '"
								+ expectedName + "'");

						if (buttonName.equals(expectedName)) {
							clickMe = thisButton;
						}
					}
					if (clickMe == null) {
						System.err
								.println("Self-test failed: test ballots don't match the given form.");
						System.exit(-1);
					}

					candButton = clickMe;

				}

				candButton.doClick(CLICK_LENGTH);
				try {
					Thread.sleep(SLEEP_LENGTH);
				} catch (InterruptedException e) {
					System.err.println("Interrupted!");
				}
				bg.validate();
				bg.repaint();

				if (j < bg.panels.length - 1) {
					bg.nextBallotLine.doClick(CLICK_LENGTH);
					try {
						Thread.sleep(SLEEP_LENGTH);
					} catch (InterruptedException e) {
						System.err.println("Interrupted!");
					}
					bg.validate();
					bg.repaint();
				}
			}

			try {
				Thread.sleep(SLEEP_LENGTH);
			} catch (InterruptedException e) {
				System.err.println("Interrupted!");
			}

			// Finish voting and confirm our votes
			bg.commitVote.doClick(CLICK_LENGTH);
			try {
				Thread.sleep(SLEEP_LENGTH);
			} catch (InterruptedException e) {
				System.err.println("Interrupted!");
			}

			while (bg.confirmPanel == null)
				Thread.yield();

			bg.confirmPanel.yesButton.doClick(CLICK_LENGTH);
			bg.validate();
			bg.repaint();

			// Loop around and vote some more!
		}

		// Okay, we're done, click the done voting button
		bg.administer.doClick(CLICK_LENGTH);
	}
}