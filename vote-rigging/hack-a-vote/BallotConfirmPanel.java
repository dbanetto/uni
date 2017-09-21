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
import java.awt.*;

/**
 * BallotConfirmPanel<p>
 *
 * The pane that pops up to inform the voter what choices he/she made.
 * Has buttons for confirm and deny. Uses a scroll pane because we
 * don't know how the heck long our ballot was.
 * @author Dave Price <dwp@alumni.rice.edu>
 */

public class BallotConfirmPanel extends JPanel {

	/**
	 * Confirm selection
	 */
	JButton yesButton;

	/**
	 * No, start over
	 */
	JButton noButton;

	/**
	 * Hashtable of cast votes
	 */
	Hashtable votes;

	/**
	 * Constructor
	 * @param votes Hashtable mapping election names to selected Candidates.
	 * @param ballots Array of String indicating which ballots are to be confirmed. Each element in this array must be a key into <tt>votes</tt>.
	 */
	public BallotConfirmPanel(Hashtable votes, String[] ballots) {
		this.votes = votes;
		this.setLayout(new BorderLayout());

		// Do the text thing
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < ballots.length; i++) {
			sb.append(ballots[i]);
			sb.append(": ");
			sb.append(((Candidate) votes.get(ballots[i])).ballotString());
			sb.append("\n");
		}

		JTextArea confirmText = new JTextArea(10, 30);
		confirmText.setText(sb.toString());
		confirmText.setEditable(false);

		confirmText.setVisible(true);
		JScrollPane confirmScroll = new JScrollPane(confirmText);
		confirmScroll.setVisible(true);
		this.add(confirmScroll, BorderLayout.CENTER);

		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setBorder(BorderFactory.createRaisedBevelBorder());
		yesButton = new JButton("Confirm these votes");
		noButton = new JButton("Start over");
		yesButton.setEnabled(true);
		noButton.setEnabled(true);
		buttonsPanel.add(yesButton);
		buttonsPanel.add(noButton);
		buttonsPanel.setVisible(true);

		JLabel header = new JLabel("Please review your votes");
		header.setVisible(true);
		this.add(header, BorderLayout.NORTH);

		this.add(buttonsPanel, BorderLayout.SOUTH);
	}
}