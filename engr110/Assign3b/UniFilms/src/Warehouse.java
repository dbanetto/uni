import java.util.ArrayList;
import java.util.List;


public class Warehouse {
	String location;
	List<Item> inventory = new ArrayList<>();
	
	public Warehouse(String location, List<Item> inventory) {
		this.location = location;
		for (Item item : inventory) {
			this.add(item);
		}
	}
	
	public String getLocation() {
		return location;
	}

	public List<Item> getInventory() {
		return inventory;
	}
	
	public void add (Item item) {
		item.setLocation(this);
		inventory.add(item);
	}
	
	public void remove(Item item) {
		if (inventory.contains(item)) {
			item.setLocation(null);
			inventory.remove(item);
		}
	}
}
