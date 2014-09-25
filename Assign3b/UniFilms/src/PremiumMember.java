import ecs100.UI;


public class PremiumMember extends RegularMember {

	public PremiumMember(String name, String address, String email,
			String password) {
		super(name, address, email, password);
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public Member upgradeMembership() {
		UI.println("Cannot upgrade a Premium Member");
		return this;
	}
	
}
