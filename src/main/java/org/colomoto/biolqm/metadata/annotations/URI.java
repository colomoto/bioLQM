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

	public String toHTML() {
		String link = this.getLink();
		if (link != null && !link.isEmpty()) {
			return "<a href='" + link + "'>" + this + "</a>";
		} else {
			return this.toString();
		}
	}

	public boolean matches(String col, String value) {
		if ((this.collection == null && col != null) || (this.collection != null && !this.collection.name.equals(col))) {
			return false;
		}
		return ((value == null && this.value == null) || (value != null && value.equals(this.value)));
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

    public String uri() {
		if (collection == null) {
			return value;
		}
		return collection.name+":"+value;
    }
}