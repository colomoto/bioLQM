package org.colomoto.biolqm.metadata.annotations;

/**
 * This contains the informations relative to a URI (a collection and the entry one wants to point at in this collection)
 *
 * @author Martin Boutroux
 */
public class URI {
	
	// variables
	private String flag;
	private String content;
	
	// constructors
	public URI(String newFlag, String newContent) {
		this.flag = newFlag;
		this.content = newContent;
	}
	
	// getters
	public String getFlag() {
		return this.flag;
	}
	public String getContent() {
		return this.content;
	}
		
	// functions
    @Override
	public boolean equals(Object obj) {
        boolean retVal = false;

		URI uri = (URI) obj;
        if (uri.getContent().equals(this.getContent())){
            retVal = true;
        }

		return retVal;
	}
	
    @Override
	public int hashCode() {

		return (int) this.getContent().hashCode();
	}
}