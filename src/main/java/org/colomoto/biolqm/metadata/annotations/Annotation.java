package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.json.JSONArray;
import org.json.JSONObject;
import scala.util.parsing.combinator.testing.Str;

import java.util.*;

/**
 * An annotation is a collection of URIs, tags and key/value pairs
 *
 * @author Martin Boutroux
 */
public class Annotation {

	public final Qualifier qualifier;
	public final List<URI> uris = new ArrayList<>();
	public final Set<String> tags = new HashSet<>();
	public final Map<String, String> keyValues = new HashMap<>();

	public Annotation(Qualifier qualifier) {
		this.qualifier = qualifier;
	}

	/**
	 * if the key doesn't exist, we create it and populate it with the value
	 * if it exists, we add the value if it is missing
	 */
	public boolean addKeyValue(String key, String value) {
		if (value == null) {
			return this.keyValues.remove(key) != null;
		}
		String old = this.keyValues.put(key, value);
		return !value.equals(old);
	}

	protected JSONObject toJSON() {
		JSONObject json = new JSONObject();
		if (this.qualifier != null) {
			json.put("qualifier", qualifier.term);
		}

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
			JSONObject keysValues = new JSONObject();
			this.keyValues.forEach(keysValues::put);
			json.put("keysvalues", keysValues);
		}
		return json;
	}

	public String toHTML() {
		if (this.isEmpty()) {
			return "";
		}

		StringBuilder sb = new StringBuilder();
		sb.append("<div class='annotations'>");
		if (!this.uris.isEmpty()) {
			sb.append("<ul class='uris'>");
			for (URI uri: this.uris) {
				sb.append("<li>").append(uri.toHTML()).append("</li>");
			}
			sb.append("</ul>");
		}

		if (!this.keyValues.isEmpty()) {
			sb.append("<ul class='keys'>");
			for (Map.Entry<String,String> e: this.keyValues.entrySet()) {
				sb.append("<li>").append(e.getKey()).append(" = ").append(e.getValue()).append("</li>");
			}
			sb.append("</ul>");
		}

		if (!this.tags.isEmpty()) {
			sb.append("<ul class='tags'>");
			for (String tag: this.tags) {
				sb.append("<li>#").append(tag).append("</li>");
			}
			sb.append("</ul>");
		}

		sb.append("</div>");

		return sb.toString();
	}

	public String getShortDescription() {
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
			String thisValues = this.keyValues.get(key);
			String geneValues = gene.keyValues.get(key);
			if (!Objects.equals(thisValues, geneValues)) {
				return false;
			}
		}
		return true;
	}
}