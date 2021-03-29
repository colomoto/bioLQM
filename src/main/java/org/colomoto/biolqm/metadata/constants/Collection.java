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
	private String nameInPattern;
	private boolean original;
	
	// constructors
	public Collection(String prefix, String newPattern, boolean newNamespaceEmbedded, boolean newOriginal) {
		this.pattern = newPattern;
		this.namespaceEmbedded = newNamespaceEmbedded;
		
		if (namespaceEmbedded) {
			String split = newPattern.split(":")[0];
			if (split.charAt(0) == '^') {
				this.nameInPattern = split.substring(1);
			} else {
				this.nameInPattern = split;
			}
			
		} else {
			this.nameInPattern = prefix;
		}
		
		this.original = newOriginal;
	}
	
	// getters
	public String getPattern() {
		return this.pattern;
	}
	public boolean getNamespaceEmbedded() {
		return this.namespaceEmbedded;
	}
	public String getNameInPattern() {
		return this.nameInPattern;
	}
	public boolean getOriginal() {
		return this.original;
	}
}