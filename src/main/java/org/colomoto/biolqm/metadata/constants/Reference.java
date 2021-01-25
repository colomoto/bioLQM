package org.colomoto.biolqm.metadata.constants;

/**
 * This contains the bibtex informations relative to an article
 *
 * @author Martin Boutroux
 */
public class Reference {
	
	// variables
	private String title;
	private String year;
	
	// constructors
	public Reference(String newTitle, String newYear) {
		this.title = newTitle;
		this.year = newYear;
	}
	
	// getters
	public String getTitle() {
		return this.title;
	}
	public String getYear() {
		return this.year;
	}
}