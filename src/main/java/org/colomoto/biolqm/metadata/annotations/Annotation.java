package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.AnnotationModule;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.*;

/**
 * An annotation is a collection of URIs, tags and key/value pairs
 *
 * @author Martin Boutroux
 */
public class Annotation {

	public final List<URI> uris = new ArrayList<>();
	public final Set<String> tags = new HashSet<>();
	public final Map<String, ArrayList<String>> keyValues = new HashMap<>();

	/**
	 * if the key doesn't exist, we create it and populate it with the value
	 * if it exists, we add the value if it is missing
	 */
	public boolean addKeyValue(String key, String value) {
		List<String> values = this.keyValues.get(key);
		if (values == null) {
			this.keyValues.put(key, new ArrayList<>(Collections.singletonList(value)));
			return true;
		}

		if (values.contains(value)) {
			return false;
		}
		values.add(value);
		return true;
	}

	protected JSONObject getJSONOfAnnotation() {
		JSONObject json = new JSONObject();

		if (!this.uris.isEmpty()) {
			JSONArray arrayURIs = new JSONArray();
			for (URI uri : this.uris) {
				JSONObject jsonURI = new JSONObject();
				jsonURI.put("type", uri.getCollection().name);
				jsonURI.put("content", uri.getValue());
				arrayURIs.put(jsonURI);
			}
			json.put("uris", arrayURIs);
		}

		if (!this.tags.isEmpty()) {
			JSONArray arrayTags = new JSONArray();
			for (String tag : this.tags) {
				arrayTags.put(tag);
			}
			json.put("tags", arrayTags);
		}

		if (!this.keyValues.isEmpty()) {
			JSONArray arrayKeysValues = new JSONArray();
			for (String key : this.keyValues.keySet()) {
				JSONObject jsonKey = new JSONObject();
				jsonKey.put("key", key);
				jsonKey.put("values", this.keyValues.get(key));
				arrayKeysValues.put(jsonKey);
			}
			json.put("keysvalues", arrayKeysValues);
		}
		return json;
	}


	protected String getShortDescription() {
		String description = "";
		switch (this.uris.size()) {
			case 0:
				break;
			case 1:
				description += "1 uri  ";
				break;
			default:
				description += this.uris.size()+" uris  ";
		}

		switch (this.tags.size()) {
			case 0:
				break;
			case 1:
				description += "1 tag, ";
				break;
			default:
				description += this.tags.size()+" tags  ";
		}

		switch (this.keyValues.size()) {
			case 0:
				break;
			case 1:
				description += "1 key";
				break;
			default:
				description += this.keyValues.size()+" keys";
		}

		return description;
	}

	public boolean isEmpty() {
		// FIXME: handle nested
		return uris.isEmpty() && tags.isEmpty() && keyValues.isEmpty();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Annotation)) {
			return false;
		}
		Annotation gene = (Annotation) obj;
		if (this.uris.size() != gene.uris.size() || this.tags.size() != gene.tags.size() || this.keyValues.size() != gene.keyValues.size()) {
			return false;
		}
		if (!(this.uris.containsAll(gene.uris) && this.tags.containsAll(gene.tags) && this.keyValues.keySet().containsAll(gene.keyValues.keySet())) ) {
			return false;
		}
		for (String key: this.keyValues.keySet()) {
			ArrayList<String> thisValues = this.keyValues.get(key);
			ArrayList<String> geneValues = gene.keyValues.get(key);
			if (thisValues.size() != geneValues.size()) {
				return false;
			}
			if (!thisValues.containsAll(geneValues)) {
				return false;
			}
		}
		return true;
	}

/*
	protected String getValue(String tab) {

		StringBuilder chaine = new StringBuilder(":\n");

		chaine.append(tab).append("\tURIs :\n");
		for (URI uri : this.listOfURIs) {
			chaine.append(tab).append("\t\t").append(uri.getContent()).append("\n");
		}

		chaine.append(tab).append("\tTags :\n");
		for (String tag : this.listOfTags) {
			chaine.append(tab).append("\t\t").append(tag).append("\n");
		}

		chaine.append(tab).append("\tKeysValues :\n");
		for (String key : this.listOfKeysValues.keySet()) {

			String joint = "";
			for (String value: this.listOfKeysValues.get(key)) {
				if (joint != "") {
					joint += ", ";
				}
				joint += value;
			}

			chaine.append(tab).append("\t\t").append(key).append(":[").append(joint).append("]\n");
		}

		return chaine.toString();
	}

	protected ArrayList<ArrayList<String>> getResources() {
		ArrayList<ArrayList<String>> resources = new ArrayList<>();

		for (URI uri : this.listOfURIs) {
			ArrayList<String> resource = new ArrayList<>();
			resource.add(uri.getFlag());
			resource.add(uri.getContent());

			resources.add(resource);
		}

		return resources;
	}

	protected boolean doesAlternativeExist(JSONObject jsonAlternative) {

		int numUriAnnotation = this.listOfURIs.size();
		if (jsonAlternative.has("uris") && !jsonAlternative.isNull("uris")) {
			JSONArray arrayURIs = jsonAlternative.getJSONArray("uris");

			int numUriJson = arrayURIs.length();
			if (numUriAnnotation == numUriJson) {

				for(int idUri = 0; idUri < arrayURIs.length(); idUri++) {
					JSONObject jsonURI = arrayURIs.getJSONObject(idUri);
					URI uri = new URI(jsonURI.getString("type"), jsonURI.getString("content"));
					if (!this.listOfURIs.contains(uri)) {
						return false;
					}
				}
			} else {
				return false;
			}
		} else if (numUriAnnotation != 0) {
			return false;
		}

		int numTagAnnotation = this.listOfTags.size();
		if (jsonAlternative.has("tags") && !jsonAlternative.isNull("tags")) {
			JSONArray arrayTags = jsonAlternative.getJSONArray("tags");

			int numTagJson = arrayTags.length();
			if (numTagAnnotation == numTagJson) {

				for(int idTag = 0; idTag < arrayTags.length(); idTag++) {
					String tag = arrayTags.getString(idTag);
					if (!this.listOfTags.contains(tag)) {
						return false;
					}
				}
			} else {
				return false;
			}
		} else if (numTagAnnotation != 0) {
			return false;
		}

		int numKeyAnnotation = this.listOfKeysValues.size();
		if (!(jsonAlternative.has("keysvalues") && !jsonAlternative.isNull("keysvalues"))) {
			return numKeyAnnotation == 0;
		}
		JSONArray arrayKeys = jsonAlternative.getJSONArray("keysvalues");

		int numKeyJson = arrayKeys.length();
		if (numKeyAnnotation != numKeyJson) {
			return false;
		}
		for(int idKey = 0; idKey < arrayKeys.length(); idKey++) {
			JSONObject key = arrayKeys.getJSONObject(idKey);
			JSONArray arrayValues = key.getJSONArray("values");

			String keyString = key.getString("key");
			if (!this.listOfKeysValues.containsKey(keyString)) {
				return false;
			}

			ArrayList<String> valuesList = this.listOfKeysValues.get(keyString);

			int numValuesAnnotation = valuesList.size();
			int numValuesJson = arrayValues.length();
			if (numValuesAnnotation != numValuesJson) {
				return false;
			}

			for (int idValue = 0; idValue < arrayValues.length(); idValue++) {
				String valueString =  arrayValues.getString(idValue);
				if (!valuesList.contains(valueString)) {
					return false;
				}
			}
		}
		return true;
	}
*/
}