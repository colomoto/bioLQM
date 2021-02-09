package org.colomoto.biolqm.metadata.constants;

/**
 * Generic class for the patterns of the collections used for the uris of the model
 *
 * @author Martin Boutroux
 */
public class Collection {
	
	// variables
	private String pattern;
	private boolean namespaceEmbedded;
	
	// constructors
	public Collection(String newPattern, boolean newNamespaceEmbedded) {
		this.pattern = newPattern;
		this.namespaceEmbedded = newNamespaceEmbedded;
	}
	
	// getters
	public String getPattern() {
		return this.pattern;
	}
	public boolean getNamespaceEmbedded() {
		return this.namespaceEmbedded;
	}
}