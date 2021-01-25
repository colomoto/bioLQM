package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.URI;
import org.colomoto.biolqm.metadata.constants.Reference;

import java.util.Map;
import java.util.HashMap;
import java.util.Map.Entry;

/**
 * One instance per model opened to keep a trace of the external metadata involved
 * When a URI concerning a reference is added, the bibtex informations concerning this reference are found on the internet and stored here
 *
 * @author Martin Boutroux
 */
public class ExternalMetadata {
	
	// variables
	public Map<URI, Reference> externalMetadata;
	
	// constructors
	public ExternalMetadata() {
		this.externalMetadata = new HashMap<URI, Reference>();
	}

	// functions
	public boolean isSetExternalMetadata(URI uri) {
		if (this.externalMetadata.containsKey(uri)) {
			return true;
		}
		return false;
	}
	
	public void updateExternalMetadata(URI uri, String title, String year) {
		if (!this.externalMetadata.containsKey(uri)) {
			this.externalMetadata.put(uri, new Reference(title, year));
		}
	}
	
	public String getDescription() {
		String help = "";
		
		for (Map.Entry<URI, Reference> entry : this.externalMetadata.entrySet()) {
			help += "\t" + entry.getKey().getIdentifier() + ": " + entry.getValue().getTitle() + ", " + entry.getValue().getYear() + "\n"; 
		}
		
		return help;
	}
}