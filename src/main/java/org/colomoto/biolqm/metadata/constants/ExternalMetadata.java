package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.URI;

import java.util.Map;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.ArrayList;

/**
 * One instance per model opened to keep a trace of the external metadata involved
 * When a URI concerning a reference is added, the bibliographic fields describing this reference are found on the internet and stored here
 *
 * @author Martin Boutroux
 */
public class ExternalMetadata {
	
	// variables
	public Map<URI, Reference> externalMetadata;
	
	// constructors
	public ExternalMetadata() {
		this.externalMetadata = new HashMap<>();
	}

	// functions
	public boolean isSetExternalMetadata(URI uri) {
		return this.externalMetadata.containsKey(uri);
	}
	
	public synchronized void updateExternalMetadata(URI uri, String title, String year, String fullName) {
		if (title.equals("") && year.equals("")) {
			this.externalMetadata.put(uri, null);
		}
		else {
			this.externalMetadata.put(uri, new Reference(title, year, fullName));
		}
	}
	
	public String getDescription() {
		StringBuilder help = new StringBuilder();
		
		for (Entry<URI, Reference> entry : this.externalMetadata.entrySet()) {
			Reference ref = entry.getValue();
			help.append("\t").append(entry.getKey().getValue().substring(4)).append(": ").append(ref.getAuthor()).append(", ").append(ref.getYear()).append(", ").append(ref.getTitle()).append("\n");
		}
		
		return help.toString();
	}
	
	public ArrayList<String> getReferencesWithYear(String year) {
		
		ArrayList<String> refs = new ArrayList<>();
		
		for (Entry<URI, Reference> entry : this.externalMetadata.entrySet()) {
			Reference ref = entry.getValue();
			
			if (ref.getYear().equals(year)) {
				URI uri = entry.getKey();
				String doi = uri.getValue();
				
				refs.add(doi);
			}
		}
		return refs;
	}
	
	public ArrayList<String> getReferencesWithKeyword(String word) {
		
		ArrayList<String> refs = new ArrayList<>();
		
		for (Entry<URI, Reference> entry : this.externalMetadata.entrySet()) {
			Reference ref = entry.getValue();
			
			if (ref.getTitle().contains(word)) {
				URI uri = entry.getKey();
				String doi = uri.getValue();
				
				refs.add(doi);
			}
		}
		return refs;
	}
	
	public Map<String, String> getListOfReferences(String inputTrimmed) {
		String[] pieces = inputTrimmed.split(" ");
		
		Map<String, String> refs = new HashMap<>();
		
		for (Entry<URI, Reference> entry : this.externalMetadata.entrySet()) {
			Reference ref = entry.getValue();
			
			boolean searched = true;
			for (String piece: pieces) {
				if (!ref.getAuthor().toLowerCase().contains(piece) && !ref.getYear().toLowerCase().contains(piece) && !ref.getTitle().toLowerCase().contains(piece)) {
					searched = false;
					break;
				}
			}
			
			if (searched) {
				String refStr = ref.getAuthor() + ";" + ref.getYear() + ";" + ref.getTitle();
				URI uri = entry.getKey();
				String uriStr = uri.getValue();
				
				refs.put(refStr, uriStr);
			}
			
			if (refs.size() == 10) {
				break;
			}
		}
		return refs;
	}
}