
public class Item {
	String name, type;
	Warehouse location;
	
	public Item(String name, String type) {
		this.name = name;
		this.type = type;
		this.location = null;
	}

	public String getName() {
		return name;
	}

	public String getType() {
		return type;
	}

	public Warehouse getLocation() {
		return location;
	}

	public void setLocation(Warehouse location) {
		this.location = location;
	}
}
