package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.Index;

import java.util.Map;
import java.util.HashMap;

/**
 * One instance per model opened containing the link between an object and its metadata for each object annotated
 *
 * @author Martin Boutroux
 */
public class ListMetadata {
	
	// variables
	public Map<Index, Metadata> listMetadata;
	
	// constructors
	public ListMetadata() {
		this.listMetadata = new HashMap<Index, Metadata>();
	}

	// functions
	public Metadata getMetadata(Index index) {
		if (this.listMetadata.containsKey(index)) {
			return this.listMetadata.get(index);
		}
		
		System.err.println("This index does not exist yet." + "\n");
		return null;
	}
}