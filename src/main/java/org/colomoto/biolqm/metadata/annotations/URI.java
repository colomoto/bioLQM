package org.colomoto.biolqm.metadata.annotations;

/**
 * This contains the informations relative to a URI (a collection and the entry one wants to point at in this collection)
 *
 * @author Martin Boutroux
 */
public class URI {
	
	// variables
	private String collection;
	private String identifier;
	
	// constructors
	public URI(String newCollection, String newIdentifier) {
		this.collection = newCollection;
		this.identifier = newIdentifier;
	}
	
	// getters
	public String getCollection() {
		return this.collection;
	}
	public String getIdentifier() {
		return this.identifier;
	}
		
	// functions
    @Override
	public boolean equals(Object obj) {
        boolean retVal = false;

		URI uri = (URI) obj;
        if (uri.getCollection().equals(this.getCollection()) && uri.getIdentifier().equals(this.getIdentifier())){
            retVal = true;
        }

		return retVal;
	}
	
    @Override
	public int hashCode() {

		return (int) this.getCollection().hashCode() + this.getIdentifier().hashCode();
	}
}