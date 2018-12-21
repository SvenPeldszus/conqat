package com.acme;

public class Customer {

	private Address address;

	public void setAddress(Address address) {
		if (this.address != null) {
			throw new IllegalArgumentException("already set");
		}
		this.address = address;
	}

	public Address getAddress() {
		return (address != null ? address : new Address());
	}

	public static class Address {
		private String street;

		public String getStreet() {
			return street;
		}

		public void setStreet(String street) {
			this.street = street;
		}
	}

}
