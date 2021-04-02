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
	private boolean original;
	
	// constructors
	public Collection(String newPattern, boolean newNamespaceEmbedded, boolean newOriginal) {
		this.pattern = newPattern;
		this.namespaceEmbedded = newNamespaceEmbedded;
		this.original = newOriginal;
	}
	
	// getters
	public String getPattern() {
		return this.pattern;
	}
	public boolean getNamespaceEmbedded() {
		return this.namespaceEmbedded;
	}
	public boolean getOriginal() {
		return this.original;
	}
}