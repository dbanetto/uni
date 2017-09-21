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
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

/**
 * The GUI frame that displays the valid PIN numbers in the system.
 * 
 * @author Dave Price <dwp@alumni.rice.edu>
 * @edited Darwin Cruz <dcruz@cs.stanford.edu>
 * @edited Brian Armstrong <barmstrong@gmail.com>
 */


public class ConsoleGUI extends JFrame {

	/**
	 * Vector of valid PINs.  Will be updated from outside.
	 */
	private Vector pinVector;

	/**
	 * Text area displaying the vector of PINs
	 */
	JTextArea pinArea;

	/*
	 * The Panel for the Console View
	 */
	JPanel consolePanel;


	/**
	 * Construct the GUI with the given vector containing PINs. The
	 * expectation is that this vector will be changed, and update()
	 * will be called when it's time to redisplay the vector.
	 *
	 * @param pinVector A vector.  Will be updated from outside as
	 * PINs are used and new PINs become valid.
	 * @param device The GraphicsDevice to be used for Full-Screen Mode.
	 */
	public ConsoleGUI(Vector pinVector) {
		super("Hack-A-Vote Console"); //Frame Title

		pinArea = new JTextArea(Console.HOW_MANY_PINS, 5);

		showConsole();

		this.pinVector = pinVector;
		this.pack();
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	/**
	 * Updates the GUI with a set of valid PIN numbers.
	 */

	public void update() {
		pinArea.setText(Console.intListString(pinVector));
		this.pack();
	}

	/*
	 * Checks available display modes until it finds a match and returns it
	 */



	/*
	 * Changes views, displays the Console view. Activated upon successful
	 * Administrator login.
	 */
	private void showConsole() {

		consolePanel = new JPanel();
		consolePanel.setLayout(new BoxLayout(consolePanel, BoxLayout.Y_AXIS));

		//Borders
		Border blackline = BorderFactory.createLineBorder(Color.black);

		JLabel pinLabel = new JLabel("Valid PIN numbers");
		pinLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		JLabel headerLabel = new JLabel("Hack-A-Vote Administrator Console");
		headerLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		pinArea.setAlignmentX(Component.CENTER_ALIGNMENT);
		pinArea.setEditable(false);

		
		consolePanel.add(headerLabel);
		consolePanel.add(Box.createRigidArea(new Dimension(0, 10))); //Spacing
		consolePanel.add(pinLabel);
		consolePanel.add(pinArea);
		TitledBorder pinBorder = BorderFactory
				.createTitledBorder(blackline, "");
		pinArea.setBorder(pinBorder);

		JButton closeButton = new JButton("Close Console");
		closeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
			}
		});
		closeButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		consolePanel.add(Box.createRigidArea(new Dimension(0, 20)));
		consolePanel.add(closeButton);

		this.getContentPane().add(consolePanel);
	}

}