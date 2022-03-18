package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.URI;

/**
 * Generic class for the patterns of the collections used for the uris of the model
 *
 * @author Martin Boutroux
 */
public class Collection {
	
	// variables
	public final String pattern;
	public final String name;
	public final boolean namespaceEmbedded;
	public final boolean original;
	
	// constructors
	public Collection(String name, String newPattern, boolean newNamespaceEmbedded, boolean newOriginal) {
		this.name = name;
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

	public String getLink(String value) {
		return "https://identifiers.org/" + this.name + value;
	}
}