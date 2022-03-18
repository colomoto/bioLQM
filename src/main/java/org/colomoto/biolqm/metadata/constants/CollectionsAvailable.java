package org.colomoto.biolqm.metadata.constants;

import org.yaml.snakeyaml.Yaml;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;
import java.io.InputStream;

/**
 * One instance per model opened to store the patterns of the collections used in uris
 * Permits to check the uris of these collections locally rathen than going on the internet
 *
 * @author Martin Boutroux
 */
public class CollectionsAvailable {
	
	public static final Map<String, Collection> COLLECTIONS;

	static {
		COLLECTIONS = new HashMap<>();
		Yaml yaml = new Yaml();
		InputStream inputStream = CollectionsAvailable.class
				.getClassLoader()
				.getResourceAsStream("collections.yaml");

		ArrayList<Map<String, String>> listOfCollections = yaml.load(inputStream);

		for (Map<String, String> collection : listOfCollections) {
			String prefix = (String) collection.get("prefix");
			String pattern = (String) collection.get("pattern");
			Boolean namespaceEmbedded = Boolean.valueOf(collection.get("namespaceEmbedded"));

			COLLECTIONS.put(prefix, new Collection(prefix, pattern, namespaceEmbedded, false));
		}
	}

	public Collection getCollection(String name) {
		return COLLECTIONS.get(name);
	}

/*
	private Set<String> namesInPatterns = new HashSet<>();
	
	// update functions
	public void updateCollections(String prefix, String pattern, boolean namespaceEmbedded) {
		if (!this.collections.containsKey(prefix)) {
			this.collections.put(prefix, new Collection(prefix, pattern, namespaceEmbedded, true));
			this.updateNamesInPatterns(prefix, pattern, namespaceEmbedded);
		}
	}
	
	private void updateNamesInPatterns(String prefix, String pattern, boolean namespaceEmbedded) {
		String nameInPattern;
		
		if (namespaceEmbedded) {
			String split = pattern.split(":")[0];
			if (split.charAt(0) == '^') {
				nameInPattern = split.substring(1);
			} else {
				nameInPattern = split;
			}
		} else {
			nameInPattern = prefix;
		}
		
		namesInPatterns.add(nameInPattern);
	}
 */
}
	