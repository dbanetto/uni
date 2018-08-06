// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP103 Assignment
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import ecs100.*;

import java.util.*;

/**
 * The FastFood game involves customers who generate orders, and the player who
 * has to fulfil the orders by assembling the right collection of food items.
 * The goal of the game is to make as much money as possible before the player
 * gets too far behind the customers and is forced to give up.
 *
 * The game presents the player with a a queue of orders in a fast food outlet.
 * The player has to fulfil the customer orders by adding the correct items to
 * the order at the head of the queue. When the order is ready, the player can
 * deliver the order, which will take it off the queue, and will add the price
 * of the order to the balance. Whenever the player adds an item to the order
 * that doesn't belong in the order, the price of the item is subtracted from
 * the balance. The player can practice by generating orders using the Practice
 * button. Once the game is started, the orders are generated automatically.
 */
public class FastFood {

	private Queue<Order> orders;
	private double balance;

	private Order making;

	public FastFood() {
		orders = new ArrayDeque<Order>();
		UI.initialise();
		UI.setImmediateRepaint(false);

		UI.addButton("Practice Order", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				orders.clear();
				making = null;
				generateOrder();
				drawOrders();
			}
		});
		UI.addButton("Add Fish", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				addItem("Fish");
				drawOrders();
			}
		});

		UI.addButton("Add Chips", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				addItem("Chips");
				drawOrders();
			}
		});

		UI.addButton("Add Burger", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				addItem("Burger");
				drawOrders();
			}
		});

		UI.addButton("Add Cream Soda", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				addItem("Cream Soda");
				drawOrders();
			}
		});
		UI.addButton("Deliver Order", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				deliverOrder();
			}
		});

		UI.addButton("Start Game", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				startGame();
			}
		});

		UI.setKeyListener(new UIKeyListener() {
			@Override
			public void keyPerformed(String key) {
				switch (key) {
				case ("f"):
				case ("q"):
					addItem("Fish");
					break;
				case ("c"):
				case ("w"):
					addItem("Chips");
					break;
				case ("e"):
				case ("b"):
					addItem("Burger");
				case ("s"):
				case ("r"):
					addItem("Cream Soda");
					break;
				case ("d"):
				case ("Space"):
					deliverOrder();
					break;
				}
			}
		});
		UI.println("Keyboard Controls : ");
		UI.println("Add Fish : Q or F");
		UI.println("Add Chips : W or C ");
		UI.println("Add Burger : E or B ");
		UI.println("Add Cream Soda : R or S ");
		UI.println("Deliver Order : Space or D ");
		drawOrders();
		this.run();

	}

	/** Create a new order and put it on the queue to be processed */
	public void generateOrder() {
		orders.add(new Order());

		if (making == null)
			making = orders.poll();
	}

	/**
	 * As long as there is an order in the queue, adds the specified item to the
	 * order at the head of the queue, If adding the item fails (ie, it isn't
	 * one of the items that are wanted by the order) then the price of the item
	 * is deducted from the current balance.
	 */
	public void addItem(String item) {
		if (making == null)
			return;

		if (!making.addItemToOrder(item) && gameRunning) {
			balance -= Order.Prices.get(item);
		}
	}

	/**
	 * As long as there is an order at the front of the queue and it is ready,
	 * take the first order off the queue, compute the price of the order, and
	 * update the total balance by adding the order price. If there is not a
	 * ready order on the queue, it prints a warning message
	 */
	public void deliverOrder() {
		if (making == null)
			return;

		if (making.isReady()) {
			if (gameRunning)
				balance += making.getPrice();
			making = orders.poll();
			this.drawOrders();
		}
	}

	/**
	 * Draw the queue of orders on the Graphics pane. Also draws the current
	 * balance in the top left corner
	 */
	public void drawOrders() {
		UI.clearGraphics(false);
		UI.setFontSize(11);
		if (gameRunning) {
			UI.drawString(String.format("Balnce $%.2f", balance), 0, 12);
		}

		if (making != null)
			making.draw(50);
		int y = 100;
		for (Order t : orders) {
			t.draw(y);
			y += 50;
		}
		UI.repaintGraphics();
	}

	// In the game version, the orders must be automatically generated.
	// The methods below will reset the queue and the current balance,
	// and will then set the gameRunning field to true. This will make
	// the run method start generating orders.
	// The run method is called from the main method, and therefore is in the
	// main
	// thread, which executes concurrently with all the GUI buttons.
	// run does nothing until the gameRunning field is set to be true
	// Once the gameRunning field is true, then it will generate orders
	// automatically,
	// every timeBetweenOrders milliseconds. It will also make the games speed
	// up
	// gradually, by steadily reducing the timeBetweenOrders.
	// You do not need to write these methods code.

	private boolean gameRunning = false;
	private long timeBetweenOrders = 5000;

	private void startGame() {
		UI.clearGraphics();
		orders.clear();
		balance = 0.0;
		timeBetweenOrders = 5000;
		gameRunning = true;
		// nextOrder = 0; nextSpeedup = 0;// I don't think they are needed
	}

	public void run() {
		long timeBetweenSpeedups = 2000;
		long timeNextOrder = 0;
		long timeNextSpeedup = 0;
		while (true) {
			this.drawOrders();
			UI.sleep(100); // Wait at least 100 milliseconds between actions.
			long now = System.currentTimeMillis();
			if (!gameRunning)
				continue;

			if (now >= timeNextOrder) {
				timeNextOrder = now + timeBetweenOrders;
				generateOrder();
				drawOrders();
			}

			if (now >= timeNextSpeedup) { // get faster steadily.
				if (timeBetweenOrders > 500)
					timeBetweenOrders -= 50;
				timeNextSpeedup = now + timeBetweenSpeedups;
			}

			if (orders.size() > 20) {
				UI.println("Oh no! You have too many orders waiting! Game over...");
				orders.clear();
				gameRunning = false;
			}
		}
	}

	public static void main(String args[]) {
		new FastFood();
	}
}
