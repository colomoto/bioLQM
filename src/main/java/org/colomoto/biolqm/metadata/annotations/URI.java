package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.Collection;

/**
 * This contains the information relative to an URI (a collection and the entry one wants to point at in this collection)
 *
 * @author Martin Boutroux
 */
public class URI {
	
	// variables
	private Collection collection;
	private String value;
	
	// constructors
	public URI(Collection collection, String value) {
		this.collection = collection;
		this.value = value;
	}
	
	public Collection getCollection() {
		return this.collection;
	}
	public String getValue() {
		return this.value;
	}

	public String getLink() {
		if (collection == null) {
			return this.value;
		}
		return this.collection.getLink(this.value);
	}
		
	// functions
    @Override
	public boolean equals(Object obj) {
		if (obj instanceof URI) {
			URI uri = (URI) obj;
			return this.value.equals(uri.value);
		}
		return false;
	}
	
    @Override
	public int hashCode() {
		return this.value.hashCode();
	}
}