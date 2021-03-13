package org.colomoto.biolqm.metadata.constants;

import org.yaml.snakeyaml.Yaml;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
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
	public Map<String, Collection> collections;
	
	// constructors
	public CollectionsAvailable() {
		this.collections = new HashMap<String, Collection>();
		
		Yaml yaml = new Yaml();
		InputStream inputStream = QualifiersAvailable.class
		  .getClassLoader()
		  .getResourceAsStream("collections.yaml");
		  
		ArrayList<Map<String, String>> listOfCollections = yaml.load(inputStream);
		
		for (Map<String, String> collection : listOfCollections) {
			String prefix = (String) collection.get("prefix");
			String pattern = (String) collection.get("pattern");
			Boolean namespaceEmbedded = Boolean.valueOf(collection.get("namespaceEmbedded"));
			
			this.collections.put(prefix, new Collection(pattern, namespaceEmbedded, false));
		}
	}
	
	// getters
	public Map<String, Collection> getCollections() {		
		return this.collections;
	}
	
	public Map<String, Collection> getOriginalCollections() {
		
		Map<String, Collection> originalCollections = new HashMap<String, Collection>();
		for (Entry<String, Collection> entry : this.collections.entrySet()) {
			String pref = entry.getKey();
			Collection coll = entry.getValue();
			if (coll.getOriginal()) {
				originalCollections.put(pref, coll);
			}
		}
		return originalCollections;
	}
	
	public void updateCollections(String prefix, String pattern, boolean namespaceEmbedded) {
		if (!this.collections.containsKey(prefix)) {
			this.collections.put(prefix, new Collection(pattern, namespaceEmbedded, true));
		}
	}
}
	