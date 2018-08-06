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
import javax.swing.table.*;
import java.awt.*;

/**
 * The table that displays the Results at the bottom of the Console..
 * 
 * @author Darwin Cruz <dcruz@cs.stanford.edu>
 */

/** 
 * TableDemo is just like SimpleTableDemo, except that it
 * uses a custom TableModel.
 */
public class ResultsTable extends JPanel {
	private boolean DEBUG = false;

	public ResultsTable() {
		super(new GridLayout(1, 0));

		JTable table = new JTable(new ResultsTableModel());
		table.setPreferredScrollableViewportSize(new Dimension(500, 70));

		//Create the scroll pane and add the table to it.
		JScrollPane scrollPane = new JScrollPane(table);

		//Add the scroll pane to this panel.
		add(scrollPane);

	}

	class ResultsTableModel extends AbstractTableModel {
		private String[] columnNames = { "Issue", "Winner", "# of Votes",
				"% of Votes" };

		private Object[][] data = {
				{ "President", "Mace Windu", "4,234,321", "54%" },
				{ "Vice-President", "Obi-Wan Kenobi", "6,432,532", "79%" },
				{ "Secretary of Defense", "Jar-Jar Binks", "5,456,112", "65%" },
				{ "Secretary of State", "Master Yoda", "9,345,552", "100%" } };

		public int getColumnCount() {
			return columnNames.length;
		}

		public int getRowCount() {
			return data.length;
		}

		public String getColumnName(int col) {
			return columnNames[col];
		}

		public Object getValueAt(int row, int col) {
			return data[row][col];
		}

		/*
		 * JTable uses this method to determine the default renderer/
		 * editor for each cell.  If we didn't implement this method,
		 * then the last column would contain text ("true"/"false"),
		 * rather than a check box.
		 */
		public Class getColumnClass(int c) {
			return getValueAt(0, c).getClass();
		}

		/*
		 * Don't need to implement this method unless your table's
		 * editable.
		 */
		public boolean isCellEditable(int row, int col) {
			//Note that the data/cell address is constant,
			//no matter where the cell appears onscreen.
			if (col < 2) {
				return false;
			} else {
				return true;
			}
		}

		/*
		 * Don't need to implement this method unless your table's
		 * data can change.
		 */
		public void setValueAt(Object value, int row, int col) {
			if (DEBUG) {
				System.out.println("Setting value at " + row + "," + col
						+ " to " + value + " (an instance of "
						+ value.getClass() + ")");
			}

			data[row][col] = value;
			fireTableCellUpdated(row, col);

			if (DEBUG) {
				System.out.println("New value of data:");
				printDebugData();
			}
		}

		private void printDebugData() {
			int numRows = getRowCount();
			int numCols = getColumnCount();

			for (int i = 0; i < numRows; i++) {
				System.out.print("    row " + i + ":");
				for (int j = 0; j < numCols; j++) {
					System.out.print("  " + data[i][j]);
				}
				System.out.println();
			}
			System.out.println("--------------------------");
		}
	}

}