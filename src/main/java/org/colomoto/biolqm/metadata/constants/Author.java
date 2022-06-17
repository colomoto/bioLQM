package org.colomoto.biolqm.metadata.constants;

import java.util.Objects;

/**
 * This contains the information relative to an author
 *
 * @author Martin Boutroux
 */
public class Author {
	
	// variables
	public final String name;
	public final String surname;
	public final String email;
	public final String organisation;
	public final String orcid;
	
	// constructors
	public Author(String newName, String newSurname, String newEmail, String newOrganisation, String newOrcid) {
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
		if (!(obj instanceof Author)) {
			return false;
		}
		
		Author author = (Author) obj;
		return (Objects.equals(this.name, author.name)
				&& Objects.equals(this.surname, author.surname)
				&& Objects.equals(this.email, author.email)
				&& Objects.equals(this.organisation, author.organisation)
				&& Objects.equals(this.orcid, author.orcid)
		);
	}
	
    @Override
	public int hashCode() {
		
		int hash = this.getName().hashCode() + this.getSurname().hashCode();
		
		if (this.getEmail() != null) { hash += this.getEmail().hashCode(); }
		if (this.getOrganisation() != null) { hash += this.getOrganisation().hashCode(); }
		if (this.getOrcid() != null) { hash += this.getOrcid().hashCode(); }

		return hash;
	}

	@Override
	public String toString() {
		return this.getName() + ", " + this.getSurname() + ", " + this.getEmail() + ", " + this.getOrganisation() + ", " + this.getOrcid();
	}
}