
public class RegularMember extends Member {

	public RegularMember(String name, String address, String email,
			String password) {
		super(name, address, email, password);
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public Member upgradeMembership() {
		// Ask if you want to upgrade
		// then upgrade
		Member upgrade = new PremiumMember(name, address, email, password);
		
		
		return upgrade;
	}
}
