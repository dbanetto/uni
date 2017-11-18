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
import java.awt.event.*;
import java.awt.*;
import java.util.*;

/**
 * PIN query panel.
 *
 * @author Dave Price <dwp@alumni.rice.edu>
 * @edited Darwin Cruz <dcruz@cs.stanford.edu>
 * @edited Brian Armstrong <barmstrong@gmail.com>
 */

public class PINQueryPanel extends JPanel {
	/**
	 * Text field in the PIN query panel where the PIN is entered.
	 */
	public JTextField pinField;
	
	/**
	 * Self reference used below
	 */
	private PINQueryPanel this2 = this;

	/**
	 * Constructs a PINQueryPanel
	 */
	public PINQueryPanel() {
		super();

		setLayout(new BorderLayout());

		JPanel topPanel = new JPanel();
		topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));

		JPanel titlePanel = new JPanel();
		titlePanel.setLayout(new BoxLayout(titlePanel, BoxLayout.X_AXIS));
		JLabel title = new JLabel("Welcome to Hack-A-Vote");
		//title.setFont(new Font("Courier", Font.BOLD, 56));
		JLabel logoLabel = new JLabel(new ImageIcon(BallotGUI.logoFile));
		titlePanel.add(title);
		titlePanel.add(Box.createRigidArea(new Dimension(100, 0)));
		titlePanel.add(logoLabel);

		JPanel enterPanel = new JPanel();
		enterPanel.setLayout(new BoxLayout(enterPanel, BoxLayout.X_AXIS));
		JLabel instructionLabel = new JLabel("Please Enter Your PIN");
		//instructionLabel.setFont(new Font("Courier", Font.BOLD, 36));
		enterPanel.add(instructionLabel);
		pinField = new JTextField("", 4); //not password field?
		pinField.setMaximumSize(new Dimension(100, 50));
		pinField.setMinimumSize(new Dimension(100, 50));
		//pinField.setFont(new Font("Courier", Font.BOLD, 36));
		enterPanel.add(Box.createRigidArea(new Dimension(50, 0)));
		enterPanel.add(pinField);

		titlePanel.setBorder(BorderFactory.createLineBorder(Color.black));
		pinField.setBorder(BorderFactory.createLineBorder(Color.black));

		topPanel.add(titlePanel);
		topPanel.add(Box.createRigidArea(new Dimension(0, 25)));
		topPanel.add(enterPanel);

		JPanel keypadPanel = new JPanel();

		JButton currButton;
		Vector keypadButtons = createKeypadButtons();
		keypadPanel.setLayout(new GridLayout(4, 3, 10, 10));
		for (int i = 0; i < keypadButtons.size(); i++) {
			currButton = (JButton) (keypadButtons.elementAt(i));
			keypadPanel.add(currButton);
			currButton.setPreferredSize(new Dimension(100, 100));
			currButton.setMinimumSize(new Dimension(100, 100));
			//currButton.setFont(new Font("Dialog", Font.BOLD, 24));
		}

		JPanel mainPanel = new JPanel();
		mainPanel.setMaximumSize(new Dimension(300, 700));
		mainPanel.setMinimumSize(new Dimension(300, 700));
		mainPanel.add(Box.createRigidArea(new Dimension(0, 50)));
		mainPanel.add(keypadPanel);

		add(topPanel, BorderLayout.NORTH);
		add(mainPanel);

	}

	public static void main(String[] args) {
		JPanel pan = new PINQueryPanel();
		JFrame frame = new JFrame("test");
		frame.getContentPane().add(pan);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}

	private Vector createKeypadButtons() {
		Vector buttons = new Vector();
		buttons.add(new JButton("7"));
		buttons.add(new JButton("8"));
		buttons.add(new JButton("9"));
		buttons.add(new JButton("4"));
		buttons.add(new JButton("5"));
		buttons.add(new JButton("6"));
		buttons.add(new JButton("1"));
		buttons.add(new JButton("2"));
		buttons.add(new JButton("3"));
		buttons.add(new JButton("Clear"));
		buttons.add(new JButton("0"));
		buttons.add(new JButton("Enter"));
		
		((JButton)buttons.get(0)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"7");
			}
		});
		((JButton)buttons.get(1)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"8");
			}
		});
		((JButton)buttons.get(2)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"9");
			}
		});
		((JButton)buttons.get(3)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"4");
			}
		});
		((JButton)buttons.get(4)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"5");
			}
		});
		((JButton)buttons.get(5)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"6");
			}
		});
		((JButton)buttons.get(6)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"1");
			}
		});
		((JButton)buttons.get(7)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"2");
			}
		});
		((JButton)buttons.get(8)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"3");
			}
		});
		((JButton)buttons.get(9)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText("");
			}
		});
		((JButton)buttons.get(10)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pinField.setText(pinField.getText()+"0");
			}
		});
		((JButton)buttons.get(11)).addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				BallotControl.checkPin(this2);
			}
		});

		return buttons;
	}

}