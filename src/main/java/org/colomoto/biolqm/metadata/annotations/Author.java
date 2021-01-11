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
        boolean retVal = false;
		
		Author author = (Author) obj;
        if (author.getName().equals(this.getName()) && author.getSurname().equals(this.getSurname()) && author.getEmail().equals(this.getEmail()) && author.getOrganisation().equals(this.getOrganisation()) && author.getOrcid().equals(this.getOrcid())){
            retVal = true;
        }

		return retVal;
	}
	
    @Override
	public int hashCode() {

		return (int) this.getName().hashCode() + this.getSurname().hashCode() + this.getEmail().hashCode() + this.getOrganisation().hashCode() + this.getOrcid().hashCode();
	}
}