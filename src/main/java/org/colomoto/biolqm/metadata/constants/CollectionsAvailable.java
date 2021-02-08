package org.colomoto.biolqm.metadata.constants;

import org.yaml.snakeyaml.Yaml;

import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.io.InputStream;

/**
 * One instance per model opened to store the patterns of the collections used in uris
 * Permits to check the uris of these collections locally rathen than going on the internet
 *
 * @author Martin Boutroux
 */
public class CollectionsAvailable {
	
	// variables
	private Map<String, String> collections;
	
	// constructors
	public CollectionsAvailable() {
		this.collections = new HashMap<String, String>();
		
		Yaml yaml = new Yaml();
		InputStream inputStream = QualifiersAvailable.class
		  .getClassLoader()
		  .getResourceAsStream("collections.yaml");
		  
		ArrayList<Map<String, String>> listOfCollections = yaml.load(inputStream);
		
		for (Map<String, String> collection : listOfCollections) {
			String prefix = (String) collection.get("prefix");
			String pattern = (String) collection.get("pattern");
			
			this.collections.put(prefix, pattern);
		}
	}
	
	// getters
	public Map<String, String> getCollections() {		
		return this.collections;
	}	
	
	public void updateCollections(String prefix, String pattern) {
		this.collections.put(prefix, pattern);
	}
}
	