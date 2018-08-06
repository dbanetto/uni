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

/**
 * Constants that help the machine know where it is.
 *
 * @author Dave Price <dwp@alumni.rice.edu>
 */
public class MachineDefaults {

	/**
	 * The date an election starts.
	 */
	public static final Date startTime = getStartTime();

	/**
	 * The date the election ends.
	 */
	public static final Date endTime = getEndTime();

	/**
	 * Whether or not to perform this time-window test.
	 */
	static final boolean timeEnable = false;

	/**
	 * Generates a Data object representing when the election starts. 
	 *
	 * @return The Date object
	 */
	private static Date getStartTime() {
		Calendar c = new GregorianCalendar();
		c.clear();
		c.set(2003, // year 
				Calendar.JUNE, // month
				15, // date
				22, // hour
				45, // minute
				00); // second
		System.out.println("start time: " + c.getTime());
		System.out.println("Now: " + new Date());
		return c.getTime();
	}

	/**
	 * Generates a Data object representing when the election ends.
	 *
	 * @return The Date object
	 */
	private static Date getEndTime() {
		Calendar c = new GregorianCalendar();
		c.clear();
		c.set(2003, // year
				Calendar.JUNE, // month
				15, // date
				23, // hour
				00, // minute
				00); // second
		return c.getTime();
	}
}