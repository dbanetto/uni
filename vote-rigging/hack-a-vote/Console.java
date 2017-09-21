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
import java.io.*;
import java.net.*;

/**
 * The administration console class - manages a list of PIN numbers that
 * are authorized to vote. The idea is that a poll worker would have this,
 * and would communicate valid PIN numbers to voters. PINs are validated
 * over the network (at the moment, using a simple unencrypted protocol).
 * When a PIN is validated, it is thrown out of the list and replaced.
 * Any of the PINs in the list are acceptable, to deal with voters using
 * PINs out of turn.
 * 
 * @author Dave Price <dwp@alumni.rice.edu>
 * @edited Darwin Cruz <dcruz@cs.stanford.edu
 * @edited Brian Armstrong <barmstrong@gmail.com>
 */

public class Console {

	/**
	 * What port we listen to for connections
	 */
	public static final int CONSOLE_PORT = 1776;

	/**
	 * How many PINs are valid at once
	 */
	public static final int HOW_MANY_PINS = 10;

	/**
	 * Random-number generator.
	 */
	static Random r;

	/**
	 * The Vector that has all the valid PIN numbers
	 */
	static Vector allPins;

	/**
	 * The GUI object that we use to display valid PINs
	 */
	static ConsoleGUI cg;

	/**
	 * Main method.  Runs the console GUI.
	 */
	public static void main(String[] args) {
		r = new Random();
		allPins = new Vector();
		for (int i = 0; i < HOW_MANY_PINS; i++)
			allPins.add(nextPIN());


		cg = new ConsoleGUI(allPins);
		cg.setVisible(true);

		ServerSocket pinSocket = null;
		try {
			pinSocket = new ServerSocket(CONSOLE_PORT);
		} catch (IOException e) {
			System.err.println("Can't run admin console: " + e);
			System.exit(-1);
		}

		/**
		 * Loop, accepting connections forever.
		 */
		while (true) {
			try {
				//System.out.println(intListString(allPins));
				cg.update();

				Socket s = pinSocket.accept();

				// go line-oriented
				PrintStream sout = new PrintStream(s.getOutputStream());
				BufferedReader sin = new BufferedReader(new InputStreamReader(s
						.getInputStream()));

				sout.println("Hack-a-Vote admin console");
				sout.println("100 Provide PIN number");

				Integer providedPIN = new Integer(sin.readLine());

				if (allPins.contains(providedPIN)) {
					// Accept this PIN
					allPins.removeElement(providedPIN);
					allPins.add(nextPIN());
					sout.println("300 PIN accepted");
				} else {
					// Deny this PIN
					sout.println("400 PIN incorrect");
				}

				s.close();
			} catch (IOException e) {
				System.err.println("Error this connection: " + e);
			}
		}
	}

	/**
	 * Get the next PIN
	 *
	 * @return the next PIN
	 */
	public static Integer nextPIN() {
		return new Integer(r.nextInt(9000) + 1000);
	}

	/**
	 * Returns the string representation of a List<Integer>.  Note
	 * that the code never actually verifies that the input is a list
	 * of integers, so more precisely, this function takes a list and
	 * returns a string containing the item.toString()s of each item
	 * on separate lines.
	 * 
	 * @param intlist A List of Integers
	 * 
	 * @return a String with the string representation of each integer on a new line
	 */
	public static String intListString(java.util.List intlist) { //List is ambiguous to java.awt and java.util	
		StringBuffer s = new StringBuffer();
		ListIterator it = intlist.listIterator();
		while (it.hasNext()) {
			s.append(it.next().toString() + "\n");
		}
		return s.toString();
	}

}