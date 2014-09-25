
public class Member {
	String name, address, email;
	String password;
	
	public Member(String name, String address, String email, String password) {
		this.name = name;
		this.address = address;
		this.email = email;
		this.password = password;
	}
	
	public Member upgradeMembership() {
		Member upgrade = new RegularMember(name, address, email, password);
		
		
		return upgrade;
	}
	
	// Getters and Setters
	
	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPassword() {
		return password;
	}
	
	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	
}
