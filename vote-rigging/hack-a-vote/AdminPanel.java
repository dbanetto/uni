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
import javax.swing.event.*;

/**
 * AdminPanel<p>
 *
 * Panel that goes on the BallotGUI that enables administrative functionality.
 * You can click to expose a password field; when the password is correct,
 * administrative buttons appear.
 *
 * @author Dave Price <dwp@alumni.rice.edu>
 */
public class AdminPanel extends JPanel {

	/**
	 * All the buttons controlled by this panel
	 */
	LinkedList allButtons;

	/**
	 * The text field where you enter the passwd
	 */
	JPasswordField passwdField;

	/**
	 * The password that enables this panel
	 */
	String adminPasswd;

	/**
	 * Construct an adminpanel.
	 * @param allButtons The LinkedList of all administrative buttons.
	 * @param adminPasswd The password you input to enable them.
	 */
	public AdminPanel(LinkedList allButtons, String adminPasswd) {
		super();
		this.allButtons = allButtons;
		this.adminPasswd = adminPasswd;

		this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

		JLabel passwdLabel = new JLabel("Administer machine");
		passwdLabel.setVisible(true);
		this.add(passwdLabel);

		JPanel bottomPanel = new JPanel();
		bottomPanel.setLayout(new BorderLayout());

		passwdField = new JPasswordField(10);
		passwdField.setVisible(true);
		bottomPanel.add(passwdField, BorderLayout.NORTH);

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(0, 1));
		bottomPanel.add(buttonPanel, BorderLayout.SOUTH);
		bottomPanel.setVisible(true);
		buttonPanel.setVisible(true);

		passwdField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				checkPasswd();
			}
		});

		ListIterator it = allButtons.listIterator();
		while (it.hasNext()) {
			JButton thisButton = (JButton) it.next();
			buttonPanel.add(thisButton);
		}

		this.add(bottomPanel);

		buttonsVisibility(false);
	}

	/**
	 * Makes buttons visible if the password is correct.
	 */
	public void checkPasswd() {
		buttonsVisibility(passwdField.getText().equals(adminPasswd));
	}

	/**
	 * Makes buttons visible.
	 *
	 * @param visibility Boolean 
	 */
	public void buttonsVisibility(boolean visibility) {
		ListIterator it = allButtons.listIterator();
		while (it.hasNext()) {
			JButton thisButton = (JButton) it.next();
			thisButton.setVisible(visibility);
		}
		this.revalidate();
	}

	/**
	 * Overrides javax.swing.JComponent.setEnabled().  Clears the
	 * password field.
	 *
	 * @param enabled Boolean indicating whether the component should
	 * be enabled or disabled.
	 */
	public void setEnabled(boolean enabled) {
		passwdField.setText("");
		passwdField.setEnabled(enabled);
		passwdField.setEditable(enabled);
		checkPasswd();
	}
}