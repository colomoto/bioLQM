package org.colomoto.biolqm.metadata.constants;

import java.util.ArrayList;

/**
 * Generic class for the qualifiers
 * This contains the characteristics of a qualifier (its ontology, its description, its definition, its probability to be used, the most useful collections for this qualifier)
 *
 * @author Martin Boutroux
 */
public class Qualifier {
	
	// variables
	public final String term;
	public final String synonym;
	public final String definition;

	private ArrayList<String> collections;

	public Qualifier(String term) {
		this(term, null, null);
	}
	
	public Qualifier(String term, String synonym, String definition) {
		this.term = term;
		this.synonym = synonym;
		this.definition = definition;
	}
	
	public ArrayList<String> getCollections() {
		return this.collections;
	}

	public boolean matches(String skel) {
		return this.term.contains(skel) || ( this.synonym != null && this.synonym.contains(skel) );
	}
}