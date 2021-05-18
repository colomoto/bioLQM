package org.colomoto.biolqm.metadata.annotations;

/**
 * This contains the informations relative to an author
 *
 * @author Martin Boutroux
 */
class Author {
	
	// variables
	private String name;
	private String surname;
	private String email;
	private String organisation;
	private String orcid;
	
	// constructors
	protected Author(String newName, String newSurname, String newEmail, String newOrganisation, String newOrcid) {
		this.name = newName;
		this.surname = newSurname;
		this.email = newEmail;
		this.organisation = newOrganisation;
		this.orcid = newOrcid;
	}
	
	// getters
	protected String getName() {
		return this.name;
	}
	protected String getSurname() {
		return this.surname;
	}
	protected String getEmail() {
		return this.email;
	}
	protected String getOrganisation() {
		return this.organisation;
	}
	protected String getOrcid() {
		return this.orcid;
	}
	
	// functions	
    @Override
	public boolean equals(Object obj) {
		
		Author author = (Author) obj;
		
		if (!author.getName().equals(this.getName()) || !author.getSurname().equals(this.getSurname())) {
			return false;
		}
		
		// test of mail
		if (!(this.getEmail() == null && author.getEmail() == null)) {
			if (this.getEmail() == null || author.getEmail() == null) {
				return false;
			}
			else if (!author.getEmail().equals(this.getEmail())) {
				return false;
			}
		}
		
		// test of organisation
		if (!(this.getOrganisation() == null && author.getOrganisation() == null)) {
			if (this.getOrganisation() == null || author.getOrganisation() == null) {
				return false;
			}
			else if (!author.getOrganisation().equals(this.getOrganisation())) {
				return false;
			}
		}
		
		// test of orcid
		if (!(this.getOrcid() == null && author.getOrcid() == null)) {
			if (this.getOrcid() == null || author.getOrcid() == null) {
				return false;
			}
			else if (!author.getOrcid().equals(this.getOrcid())) {
				return false;
			}
		}
		
		return true;
	}
	
    @Override
	public int hashCode() {
		
		int hash = this.getName().hashCode() + this.getSurname().hashCode();
		
		if (this.getEmail() != null) { hash += this.getEmail().hashCode(); }
		if (this.getOrganisation() != null) { hash += this.getOrganisation().hashCode(); }
		if (this.getOrcid() != null) { hash += this.getOrcid().hashCode(); }

		return hash;
	}
}