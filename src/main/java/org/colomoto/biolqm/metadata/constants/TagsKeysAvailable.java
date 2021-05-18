package org.colomoto.biolqm.metadata.constants;

import java.util.HashSet;
import java.util.HashMap;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;

/**
 * One instance per model opened to store the keys and tags used in the model
 *
 * @author Martin Boutroux
 */
public class TagsKeysAvailable {
	
	// variables
	public Set<String> tags;
	public Map<String, ArrayList<String>> keysValues;
	
	// constructors
	public TagsKeysAvailable() {
		this.tags = new HashSet<String>();
		this.keysValues = new HashMap<String, ArrayList<String>>();
	}
	
	// functions	
	public void updateTagsAvailable(String newTag) {
		this.tags.add(newTag);
	}
	
	public void updateKeysValuesAvailable(String newKey, ArrayList<String> newValues) {
		// if the key doesn't exist, we create it and populate it with the values
		// if it exists, we add to the keys the values missing
		
		if (!this.keysValues.containsKey(newKey)) {
			this.keysValues.put(newKey, newValues);
		}
		else {
			ArrayList<String> currentValues = this.keysValues.get(newKey);
			
			for (String newValue: newValues) {
				if (!currentValues.contains(newValue)) {
					currentValues.add(newValue);
				}
			}
		}
	}
}
