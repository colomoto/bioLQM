package org.colomoto.biolqm.metadata.constants;

import org.yaml.snakeyaml.Yaml;

import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.io.InputStream;

/**
 * One instance per model opened to store the patterns of the collections used in uris
 * Permits to check the uris of these collections locally rather than going on the internet
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
		if (!COLLECTIONS.containsKey(name)) {
			COLLECTIONS.put(name, new Collection(name, null, false, false));
		}
		return COLLECTIONS.get(name);
	}
}
	