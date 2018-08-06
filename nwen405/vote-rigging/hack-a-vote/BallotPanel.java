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
 * BallotPanel<p>
 * The Swing GUI for the actual selection of a ballot.
 * 
 * @author Dave Price <dwp@alumni.rice.edu>
 */

public class BallotPanel extends JPanel {

	/**
	 * The name of this election
	 */
	String title;

	/**
	 * The button group we represent here
	 */
	ButtonGroup group;

	/**
	 * hashtable mapping ButtonModels to Candidates
	 */
	Hashtable buttonMap;

	/**
	 * The list of Candidates for this panel
	 */
	java.util.List ballotEntries;

	/**
	 * The hidden none-selected button
	 */
	JToggleButton noneButton;

	/**
	 * Constructor
	 * 
	 * @param title A description of this election
	 * @param ballotEntries A list of Candidate indicating who is on this ballot line
	 */
	public BallotPanel(String title, java.util.List ballotEntries) {

		super();

		this.title = title;
		group = new ButtonGroup();
		buttonMap = new Hashtable();
		this.ballotEntries = ballotEntries;

		noneButton = new JToggleButton("Hidden button");
		group.add(noneButton);

		this.setLayout(new GridLayout(0, 1));

		this.add(new JLabel(title));

		LinkedList buttons = new LinkedList();

		// Build up our Buttons and map them.
		ListIterator it = ballotEntries.listIterator();
		while (it.hasNext()) {
			Candidate c = (Candidate) it.next();

			// Create a new toggle button for that candidate
			JToggleButton cbutton = new JToggleButton(c.ballotString());

			// Map that button's model (which is what ButtonGroup gives us when we
			// ask) to the Candidate in the hashtable - this lets us ask who is
			// selected
			buttonMap.put(cbutton.getModel(), c);

			// Do GUI stuff
			group.add(cbutton);
			cbutton.setVisible(true);
			this.add(cbutton);
		}

		noneButton.setSelected(true);
		this.setMinimumSize(new Dimension(200, 300));
		this.setVisible(true);
	}

	/**
	 * Causes this panel to revert to a none-selected state
	 */
	public void unselect() {
		noneButton.setSelected(true);
	}

	/**
	 * Sets all the buttons in this panel to enabled (true) or
	 * disabled (false).
	 *
	 * @param b Boolean indicating whether to enabled (true) or
	 * disabled (false) the buttons
	 */
	public void setEnabled(boolean b) {
		Enumeration e = buttonMap.keys();
		while (e.hasMoreElements()) {
			ButtonModel thisButton = (ButtonModel) e.nextElement();
			thisButton.setEnabled(b);
		}
	}

	/**
	 * Returns which candidate has been selected, or null
	 * if no candidate is yet selected
	 *
	 * @return The candidate
	 */
	public Candidate selectedCandidate() {
		if (group.getSelection() == noneButton.getModel())
			return null;
		else
			return (Candidate) buttonMap.get(group.getSelection());
	}

}