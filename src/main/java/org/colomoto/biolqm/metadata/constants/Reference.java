package org.colomoto.biolqm.metadata.constants;

/**
 * This contains the bibtex informations relative to an article
 *
 * @author Martin Boutroux
 */
public class Reference {
	
	// variables
	private String bibtex;
	
	// constructors
	public Reference(String newBibtex) {
		this.bibtex = newBibtex;
	}
	
	// getters
	public String getBibtex() {
		return this.bibtex;
	}
	
	// setters
	public void setBibtex(String newBibtex) {
		this.bibtex = newBibtex;
	}
}