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
import java.awt.event.*;

/**
 * BallotGUI<p>
 *
 * The Swing GUI for a balloting machine.
 *
 * @author Dave Price <dwp@alumni.rice.edu>
 * @edited Brian Armstrong <barmstrong@gmail.com>
 */

public class BallotGUI extends JPanel {

	/* GUI components for main frame */

	/**
	 * Next ballot line
	 */
	JButton nextBallotLine;

	/**
	 * Previous ballot line
	 */
	JButton prevBallotLine;

	/**
	 * Commit your vote
	 */
	JButton commitVote;

	/**
	 * Enter administrative mode
	 */
	JButton administer;

	/**
	 * Panel that manages 'administer' button
	 */
	AdminPanel adminPanel;

	/**
	 * Panel containing the ballot panels and the PIN query panel
	 */
	JPanel ballotPanelHolder;

	/**
	 * Panel For confirming your votes before they are recorded.
	 * Never used here but referenced from BallotControl.
	 */
	BallotConfirmPanel confirmPanel;

	/**
	 * The list of panels for the individual elections.
	 */
	BallotPanel[] panels;

	/**
	 * The current Ballot panel
	 */
	BallotPanel activePanel;

	/**
	 * The PIN query panel
	 */
	PINQueryPanel pq;

	/**
	 * The current ballot panel.  An index into the panels array.
	 */
	int displayedPanel;

	/**
	 * The file containing an image of the Hack-a-vote logo
	 */
	public static final String logoFile = "img/flag.jpg";

	/**
	 * The name of this program ("Hack-a-vote").
	 */
	public static final String NAME = "Hack-a-Vote";

	/**
	 * Constructor
	 * @param ballotPanels A List of BallotPanels, in order of presentation.
	 */
	public BallotGUI(BallotPanel[] ballotPanels) {

		// build ourselves
		super();
		this.setLayout(new BorderLayout());

		panels = ballotPanels;

		// BALLOTING PANE
		ballotPanelHolder = new JPanel();
		ballotPanelHolder.setLayout(new BorderLayout());
		ballotPanelHolder.setBorder(BorderFactory.createLoweredBevelBorder());

		// Add the dummy panel with status text

		JLabel ballotLabel = new JLabel("Ballot 1 of " + panels.length);
		ballotLabel.setVisible(true);

		displayedPanel = 0;
		activePanel = null;
		ballotPanelHolder.add(panels[displayedPanel], BorderLayout.NORTH);
		ballotPanelHolder.add(ballotLabel, BorderLayout.SOUTH);

		ballotPanelHolder.setVisible(true);

		this.add(ballotPanelHolder, BorderLayout.CENTER);

		// RIGHT SIDE
		JPanel rightSide = new JPanel();
		rightSide.setLayout(new BoxLayout(rightSide, BoxLayout.Y_AXIS));
		JLabel logoLabel = new JLabel(new ImageIcon(logoFile));
		logoLabel.setVisible(true);
		rightSide.add(logoLabel);
		rightSide.setVisible(true);
		this.add(rightSide, BorderLayout.EAST);

		JLabel nameLabel = new JLabel(NAME);
		nameLabel.setVisible(true);
		rightSide.add(nameLabel);

		JLabel captionLabel = new JLabel("Trust us, it works fine");
		captionLabel.setVisible(true);
		rightSide.add(captionLabel);

		administer = new JButton("Election over");

		LinkedList adminButtons = new LinkedList();
		adminButtons.add(administer);
		adminPanel = new AdminPanel(adminButtons, "secret");

		//	administer.setVisible(true);
		Component spacer = Box.createVerticalStrut(30);
		rightSide.add(spacer);
		rightSide.add(adminPanel);

		// BOTTOM BUTTONS
		JPanel bottom = new JPanel(new GridLayout(1, 0));
		bottom.setBorder(BorderFactory.createEtchedBorder());

		prevBallotLine = new JButton("Previous", new ImageIcon(
				"img/nav_prev.gif"));
		prevBallotLine.setVisible(true);
		bottom.add(prevBallotLine);

		nextBallotLine = new JButton("Next", new ImageIcon("img/nav_next.gif"));
		nextBallotLine.setHorizontalTextPosition(AbstractButton.LEADING);
		nextBallotLine.setVisible(true);
		bottom.add(nextBallotLine);

		commitVote = new JButton("Finish voting",
				new ImageIcon("img/check.jpg"));
		commitVote.setVisible(true);
		commitVote.setEnabled(false);
		bottom.add(commitVote);

		bottom.setVisible(true);
		this.add(bottom, BorderLayout.SOUTH);

		// Add the action listeners to allow for surfing between ballots
		nextBallotLine.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				movePanel(true);
			}
		});
		prevBallotLine.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				movePanel(false);
			}
		});

		// FINISH UP
		this.setPreferredSize(new Dimension(500, 400));
		this.setVisible(true);

		// Turn our buttons off until the controller says it's okay
		this.setEnabled(false);

	}

	/**
	 * Display the first panel again.
	 */
	public void displayFirst() {
		displayedPanel = 1;
		movePanel(false);
	}

	/**
	 * Set the appropriate buttons to be enabled.
	 *
	 * @param b If true, enable panels and appropriate buttons.  If
	 * false, disable panels and both buttons.
	 */
	public void setEnabled(boolean b) {
		if (b) {
			if (displayedPanel <= 0)
				prevBallotLine.setEnabled(false);
			else
				prevBallotLine.setEnabled(true);
			if (displayedPanel < panels.length - 1)
				nextBallotLine.setEnabled(true);
			else
				nextBallotLine.setEnabled(false);
			adminPanel.setEnabled(true);
		} else {
			prevBallotLine.setEnabled(false);
			nextBallotLine.setEnabled(false);
			adminPanel.setEnabled(false);
			commitVote.setEnabled(false);
		}
		for (int i = 0; i < panels.length; i++)
			panels[i].setEnabled(b);
	}

	/**
	 * Process a 'next' or 'previous' click.
	 * @param goNext is true if 'next' was clicked, false if 'previous'.
	 */
	private void movePanel(boolean goNext) {
		int oldD = displayedPanel;
		if (goNext)
			displayedPanel++;
		else
			displayedPanel--;
		if (displayedPanel <= 0)
			prevBallotLine.setEnabled(false);
		else
			prevBallotLine.setEnabled(true);

		if (displayedPanel >= panels.length - 1)
			nextBallotLine.setEnabled(false);
		else
			nextBallotLine.setEnabled(true);

		ballotPanelHolder.removeAll();
		panels[displayedPanel].setVisible(true);
		JLabel newLower = new JLabel("Ballot " + (displayedPanel + 1) + " of "
				+ panels.length);
		newLower.setVisible(true);
		ballotPanelHolder.add(panels[displayedPanel], BorderLayout.NORTH);
		ballotPanelHolder.add(newLower, BorderLayout.SOUTH);
		ballotPanelHolder.validate();
		ballotPanelHolder.repaint();
		
	}

	/**
	 * Display the PIN query panel specified
	 * @param pq The PINQueryPanel to display
	 */
	public void displayPINQuery(PINQueryPanel pq) {
		ballotPanelHolder.removeAll();
		panels[displayedPanel].setVisible(false);
		ballotPanelHolder.add(pq, BorderLayout.CENTER);
		ballotPanelHolder.revalidate();
		ballotPanelHolder.repaint();
	}

	/**
	 * Main method.  Don't use.  Run BallotControl instead.
	 */
	public static void main(String[] args) {
		System.err.println("Use BallotControl instead");
	}
}