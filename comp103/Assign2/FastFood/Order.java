// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP103 Assignment
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ecs100.UI;

public class Order {

	public static final Map<String, Double> Prices;
	static {
		Prices = new HashMap<String, Double>();
		Prices.put("Fish", 2.50);
		Prices.put("Chips", 1.50);
		Prices.put("Burger", 5.00);
		Prices.put("Cream Soda", 2.00);
	}

	/** the items that are wanted for the order */
	private int wantsFish;
	private int wantsChips;
	private int wantsBurger;
	private int wantsCream;

	/** the items that have been added and are ready in the order */
	private int hasFish;
	private int hasChips;
	private int hasBurger;
	private int hasCream;

	public Order() {
		do {
			wantsFish = (Math.random() > 0.4 ? 1 : 0);
			wantsChips = (Math.random() > 0.4 ? 1 : 0);
			wantsBurger = (Math.random() > 0.4 ? 1 : 0);
			wantsCream = (Math.random() > 0.4 ? 1 : 0);

			if (wantsFish != 0 || wantsChips != 0 || wantsBurger != 0) {
				int choice = (int) (Math.random() * 4);
				if (choice == 0)
					wantsFish += (int) (Math.random() * 5);
				else if (choice == 1)
					wantsChips += (int) (Math.random() * 5);
				else if (choice == 2)
					wantsBurger += (int) (Math.random() * 5);
				else if (choice == 2)
					wantsCream += (int) (Math.random() * 5);
			}
		} while (this.getPrice() == 0);
	}

	/**
	 * The order is ready as long as there every item that is wanted is also
	 * ready.
	 */
	public boolean isReady() {
		return (wantsFish == hasFish && wantsChips == hasChips
				&& wantsBurger == hasBurger && wantsCream == hasCream);
	}

	/**
	 * If the item is wanted but not already in the order, then put it in the
	 * order and return true, to say it was successful. If the item not wanted,
	 * or is already in the order, then return false to say it failed.
	 */
	public boolean addItemToOrder(String item) {
		if (item.equals("Fish")) {
			if (wantsFish > hasFish) {
				hasFish += 1;
				return true;
			}
		} else if (item.equals("Chips")) {
			if (wantsChips > hasChips) {
				hasChips += 1;
				return true;
			}
		} else if (item.equals("Burger")) {
			if (wantsBurger > hasBurger) {
				hasBurger += 1;
				return true;
			}
		} else if (item.equals("Cream Soda")) {
			if (wantsCream > hasCream) {
				hasCream += 1;
				return true;
			}
		}
		return false;
	}

	/**
	 * Computes and returns the price of an order. Core: Uses constants: 2.50
	 * for fish, 1.50 for chips, 5.00 for burger to add up the prices of each
	 * item Completion: Uses a map of prices to look up prices
	 */
	public double getPrice() {
		double price = 0;
		price += wantsFish * Prices.get("Fish");
		price += wantsChips * Prices.get("Chips");
		price += wantsBurger * Prices.get("Burger");
		price += wantsCream * Prices.get("Cream Soda");
		return price;
	}

	public void draw(int y) {
		int xpos = 10;
		int imgHeight = 50; // Image height

		for (int i = (wantsFish - hasFish); i < wantsFish; i++) {
			UI.drawImage("Fish.png", xpos, y);
			xpos += imgHeight;
		}

		for (int i = 0; i < (wantsFish - hasFish); i++) {
			UI.drawImage("Fish-grey.png", xpos, y);
			xpos += imgHeight;
		}

		for (int i = (wantsChips - hasChips); i < wantsChips; i++) {
			UI.drawImage("Chips.png", xpos, y);
			xpos += imgHeight;
		}

		for (int i = 0; i < (wantsChips - hasChips); i++) {
			UI.drawImage("Chips-grey.png", xpos, y);
			xpos += imgHeight;
		}

		for (int i = (wantsBurger - hasBurger); i < wantsBurger; i++) {
			UI.drawImage("Burger.png", xpos, y);
			xpos += imgHeight;
		}

		for (int i = 0; i < (wantsBurger - hasBurger); i++) {
			UI.drawImage("Burger-grey.png", xpos, y);
			xpos += imgHeight;
		}

		for (int i = (wantsCream - hasCream); i < wantsCream; i++) {
			UI.drawImage("Cream.png", xpos, y);
			xpos += imgHeight;
		}

		for (int i = 0; i < (wantsCream - hasCream); i++) {
			UI.drawImage("Cream-grey.png", xpos, y);
			xpos += imgHeight;
		}
	}
}
