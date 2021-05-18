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
	private String author;
	
	// constructors
	public Reference(String newTitle, String newYear, String fullName) {
		this.title = newTitle;
		this.year = newYear;
		this.author = fullName;
	}
	
	// getters
	public String getTitle() {
		return this.title;
	}
	public String getYear() {
		return this.year;
	}
	public String getAuthor() {
		return this.author;
	}
}