import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

import ecs100.*;

public class UniFilmsSystem {

	private List<Member> members = new ArrayList<>();
	private List<Warehouse> warehouses = new ArrayList<>();
	private Member current = null;

	public UniFilmsSystem() {
		// Populate the databases
		members.add(new Member("David", "Wellington", 
				"david@site.com", "1123"));
		members.add(new RegularMember("David", "Wellington", 
				"david@reg.com", "1123"));
		members.add(new PremiumMember("David", "Wellington", 
				"david@prem.com", "1123"));
		
		warehouses.add(new Warehouse("Hamilton", Arrays.asList(
			new Item("Call of Duty 18","Film"),
			new Item("2Fast4u","Game"),
			new Item("Investment Banking","Film")
		)));
		

		warehouses.add(new Warehouse("Wellington", Arrays.asList(
			new Item("Minecraft","Game"),
			new Item("Gravity","Film"),
			new Item("Dues Ex","Game")
		)));
	}

	public void start() {
		UI.initialise();

		UI.addButton("Login", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				String email = UI.askString("Email    : ");
				String password = UI.askString("Password : ");
				login(email, password);
			}
		});
		UI.addButton("Logout", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				logout();
			}
		});
		
		// Separator button
		UI.addButton("", new UIButtonListener() {public void buttonPerformed(String n){}});
		
		UI.addButton("Browse", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				browse();
			}
		});
		
		UI.addButton("Update Details", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				if (current != null) {
					//current.updateDetails();
				} else {
					UI.println("Must be logged in to " + name);
				}
			}
		});
		UI.addButton("Upgrade Membership", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				if (current != null) {
					// Replace item in Members with new Member
					//current.upgradeMembership();
				} else {
					UI.println("Must be logged in to " + name);
				}
			}
		});
		UI.addButton("Downgrade Membership", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				if (current != null) {
					if (current instanceof RegularMember) {
						// Replace item in Members with new Member
						//current.upgradeMembership();
					} else {
						UI.println("Must be atleast a Member to " + name);
					}
				} else {
					UI.println("Must be logged in to " + name);
				}
			}
		});
		
	}

	public boolean login(String email, String password) {
		if (current != null) {
			UI.println("You must log out before loggin in");
			return false;
		}

		for (Member user : members) {
			if (user.getEmail().equals(email)
					&& user.getPassword().equals(password)) {
				UI.printf("Welcome %s\n", user.getName());
				current = user;
				return true;
			}
		}
		UI.println("email or password was incorrect");
		return false;
	}
	
	public boolean logout() {
		if (current == null) {
			UI.println("You must be logged in to log out");
			return false;
		}
		
		UI.printf("%s has logged out\n", current.getName());
		current = null;
		return true;
	}
	
	public void browse() {
		if (current == null) {
			UI.println("Must be logged in to browse");
			return;
		}
		// FIXME : Print current location's warehouse
		for (Warehouse warehouse : warehouses) {
			for (Item item : warehouse.getInventory()) {
				UI.printf("%s @ %s Type: %s\n", item.getName(),warehouse.getLocation() , item.getType());
			}
		}
	}
}
