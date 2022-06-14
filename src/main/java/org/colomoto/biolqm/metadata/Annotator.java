package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.metadata.annotations.*;
import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.colomoto.biolqm.metadata.validations.PatternValidator;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;


/**
 * Create, read and modify the annotations of a model.
 *
 * This object provides access to the annotations stored in the {@link AnnotationModule}.
 * It exposes methods to select the annotated object (the model itself, a component or an interaction),
 * and to access and edit the annotation content.
 *
 * @author Martin Boutroux
 * @author Aurelien Naldi
 */
public class Annotator<N> {

	private final AnnotationModule mod;

	private AnnotationTarget target = AnnotationTarget.Model;
	private Object selected = null;
	private Qualifier qualifier = null;
	private int alternative = 0;
	private Annotation cur_annotation = null;

	public Annotator(AnnotationModule mod) {
		this.mod = mod;
	}

	/**
	 * Check if any element is annotated in this model
	 * @return true if no annotations exist on the model itself or any of its elements
	 */
	public boolean isEmpty() {
		return this.mod.isEmpty();
	}

	/**
	 * Retrieve the full metadata associated to the current selection.
	 * @return the existing metadata or null if it is not defined.
	 */
	private Metadata getMetadata() {
		return this.mod.getAnnotation(this.selected);
	}

	/**
	 * Retrieve the annotation associated to the current selection and qualifier.
	 * @return the existing annotation or null if it is not defined.
	 */
	private Annotation getAnnotation() {
		Metadata mdt =  this.mod.getAnnotation(this.selected);
		if (mdt == null) {
			return null;
		}
		return mdt.getAnnotation(this.qualifier, this.alternative);
	}

	/**
	 * Retrieve or create the annotations associated to the current selection.
	 * @return the existing annotation or a new one if it was not defined.
	 */
	private Metadata ensureMetadata() {
		return this.mod.ensureMetadata(this.selected);
	}

	private Annotation ensureAnnotation() {
		if (cur_annotation == null) {
			Metadata meta = this.ensureMetadata();
			cur_annotation = meta.ensureAnnotation(this.qualifier, this.alternative);
		}
		return cur_annotation;
	}

	private Annotator<N> setSelection(AnnotationTarget target, Object selected) {
		this.target = target;
		this.selected = selected;
		this.qualify(null);
		return this;
	}

	public Annotator<N> onModel() {
		return setSelection(AnnotationTarget.Model, null);
	}

	public Annotator<N> node(N node) {
		if (node == null) {
			return this.onModel();
		}
		return this.setSelection(AnnotationTarget.Component, node);
	}

	public Annotator<N> edge(N src, N tgt) {
		this.edge(new Pair<>(src, tgt));
		return this;
	}

	public Annotator<N> edge(Pair<N> edge) {
		if (edge == null) {
			onModel();
			return this;
		}
		return this.setSelection(AnnotationTarget.Interaction, edge);
	}

	public Annotator<N> qualify(String qualifier) {
		this.qualify(qualifier, 0);
		return this;
	}

	public Annotator<N> qualify(String qualifier, int alternative) {
		this.alternative = alternative;
		this.cur_annotation = null;
		this.qualifier = this.mod.ensureQualifier(this.target, qualifier);
		return this;
	}

	private Annotator<N> nested() {
		// FIXME: implement nested annotations
		throw new RuntimeException("Nested annotations are not yet fully supported");
	}

	/**
	 * Guess the annotation type and add it
	 *
	 * @param s a string representing the annotation
	 */
	public Annotator<N> annotate(String s) {
		// FIXME: guess annotation type and add it in the right spot
		Matcher m = PatternValidator.matchTag(s);
		if (m.matches()) {
			return this.tag(m.group(1));
		}

		m = PatternValidator.matchKeyValue(s);
		if (m.matches()) {
			return this.put(m.group(1), m.group(2));
		}

		m = PatternValidator.matchCollection(s);
		if (m.matches()) {
			return this.identifier(m.group(1), m.group(2));
		}

		// TODO: error
		System.err.println("Unrecognized annotation");
		return this;
	}

	// Fill annotations
	public Annotator<N> tag(String tag) {
		if (this.ensureAnnotation().tags.add(tag)) {
			this.mod.useTag(tag);
		}
		return this;
	}

	public Annotator<N> put(String key, String value) {
		if (this.ensureAnnotation().addKeyValue(key, value)) {
			this.mod.useKey(key);
		}
		return this;
	}

	public Annotator<N> identifier(String col, String entry) {
		this.ensureAnnotation().uris.add( new URI(this.mod.getCollection(col), entry) );
		return this;
	}

	public boolean hasTags() {
		Annotation annot = getAnnotation();
		return annot != null && !annot.tags.isEmpty();
	}
	public boolean hasTag(String tag) {
		Annotation annot = getAnnotation();
		return annot != null && annot.tags.contains(tag);
	}
	public boolean hasEntries() {
		Annotation annot = getAnnotation();
		return annot != null && !annot.keyValues.isEmpty();
	}
	public boolean hasURIs() {
		Annotation annot = getAnnotation();
		return annot != null && !annot.uris.isEmpty();
	}

	public Iterable<String> tags() {
		Annotation annot = getAnnotation();
		if (annot == null) {
			return null;
		}
		return annot.tags;
	}

	public Iterable<Qualifier> qualifiers() {
		Metadata mdt = getMetadata();
		if (mdt == null) {
			return null;
		}
		return mdt.qualifiers();
	}

	public Iterable<Map.Entry<String, String>> entries() {
		Annotation annot = getAnnotation();
		if (annot == null) {
			return null;
		}
		return annot.keyValues.entrySet();
	}

	public Iterable<URI> uris() {
		Annotation annot = getAnnotation();
		if (annot == null) {
			return null;
		}
		return annot.uris;
	}

	public String getNotes() {
		Metadata mdt = getMetadata();
		if (mdt == null) {
			return null;
		}
		return mdt.getNotes();
	}

	public void setNotes(String notes) {
		this.ensureMetadata().setNotes(notes);
	}

	/**
	 * Write the JSON of all the annotations in the model
	 */
	public JSONObject writeAnnotationsInJSON() {
		JSONObject json = new JSONObject();

		Metadata mdt = this.mod.getAnnotation(null);
		if (mdt != null) {
			// FIXME: export collection metadata
			// this.modelMetadata.exportCollectionsMetadata(json);
			JSONObject jsonModel = mdt.toJSON();
			if (jsonModel != null) {
				json.put("model", jsonModel);
			}
		}

		// Create separate storage for node and edge annotations
		JSONObject jsonNodes = new JSONObject();
		JSONObject jsonEdges = new JSONObject();

		// Dispatch all annotations
		for (Object o: this.mod.annotated()) {
			// TODO: check that the node/edge exists!
			if (o instanceof Pair) {
				fillJSONForEdge((Pair<N>)o, jsonEdges);
			} else {
				fillJSONForNode((N)o, jsonNodes);
			}
		}

		// Save the collected annotations
		json.put("nodes", jsonNodes);
		json.put("edges", jsonEdges);

		// Add the current date to the modified qualifier
		boolean modifiedAdded = false;
		if (json.has("annotation") && !json.isNull("annotation")) {
			JSONArray arrayQualifiers = json.getJSONArray("annotation");

			for(int idQualifier = 0; idQualifier < arrayQualifiers.length(); idQualifier++) {
				JSONObject jsonQualifier = arrayQualifiers.getJSONObject(idQualifier);
				String qualifierName = jsonQualifier.getString("qualifier");

				// if modified field already exists
				if (qualifierName.equals("modified")) {
					JSONArray arrayAlternatives = jsonQualifier.getJSONArray("alternatives");
					arrayAlternatives.remove(0);

					JSONObject jsonDate = new JSONObject();
					jsonDate.put("date", LocalDate.now().toString());
					arrayAlternatives.put(jsonDate);
					modifiedAdded = true;
				}
			}
			// if modified field doesn't exist yet
			if (!modifiedAdded) {
				JSONObject jsonGlobalDate = new JSONObject();
				JSONArray arrayAlternatives = new JSONArray();
				JSONObject jsonDate = new JSONObject();
				jsonDate.put("date", LocalDate.now().toString());
				arrayAlternatives.put(jsonDate);

				jsonGlobalDate.put("qualifier", "modified");
				jsonGlobalDate.put("alternatives", arrayAlternatives);
				jsonGlobalDate.put("type", "DateAnnotation");

				arrayQualifiers.put(jsonGlobalDate);
			}
		}

		return json;
	}

	private void fillJSONForNode(N ni, JSONObject jsonNodes) {
		this.node(ni);
		Metadata meta = this.getMetadata();
		if (meta != null) {
			JSONObject jsonNode = meta.toJSON();
			if (jsonNode != null) {
				jsonNodes.put(ni.toString(), jsonNode);
			}
		}
	}

	private void fillJSONForEdge(Pair<N> nip, JSONObject jsonEdges) {
		this.edge(nip);
		Metadata meta = this.getMetadata();
		if (meta == null || meta.isEmpty()) {
			return;
		}
		JSONObject jsonEdge = meta.toJSON();
		if (jsonEdge != null) {
			jsonEdges.put(nip.node1+":"+nip.node2, jsonEdge);
		}
	}
	
	/**
	 * Put all the annotations of the json in the model
	 * 
	 * @param json the content of the json file
	 * @param nodeMap
	 */
	public void readAnnotationsFromJSON(JSONObject json, Map<String, N> nodeMap) {
		// FIXME: import collection metadata
//		if (json.has("collections") && !json.isNull("collections")) {
//			metadataModel.importCollectionsMetadata(json.getJSONArray("collections"));
//		}

		JSONObject jmodel = json.optJSONObject("model");
		this.onModel().addJSON(jmodel);

		readNodesJSON(json.optJSONObject("nodes"), nodeMap);
		readEdgesJSON(json.optJSONObject("edges"), nodeMap);
	}

	private void readNodesJSON(JSONObject jnodes, Map<String, N> nodeMap) {
		if (jnodes == null || jnodes.isEmpty()) {
			return;
		}

		// Import the metadata for each node
		for (String key : jnodes.keySet()) {
			JSONObject cur = jnodes.getJSONObject(key);
			if (cur == null || cur.isEmpty()) {
				continue;
			}

			N node = nodeMap.get(key);
			if (node == null) {
				System.err.println("The node " + key + " has no equivalent in the model so its annotations couldn't be imported.");
				continue;
			}
			this.node(node);
			this.addJSON(cur);
		}
	}

	private void readEdgesJSON(JSONObject jedges, Map<String, N> nodeMap) {
		if (jedges == null || jedges.isEmpty()) {
			return;
		}

		// we import the metadata concerning each edge
		for (String key : jedges.keySet()) {
			JSONObject cur = jedges.getJSONObject(key);
			if (cur == null || cur.isEmpty()) {
				continue;
			}

			String[] split_key = key.split(":");
			if (split_key.length != 2) {
				System.err.println("The key " + key + " does not look like an edge identifier.");
				continue;
			}
			N node1 = nodeMap.get(split_key[0]);
			N node2 = nodeMap.get(split_key[1]);
			if (node1 == null || node2 == null) {
				System.err.println("The node " + key + " has no equivalent in the model so its annotations couldn't be imported.");
				continue;
			}
			this.edge(node1, node2);
			this.addJSON(cur);
		}
	}

	private void addJSON(JSONObject json) {
		addJSONNotes(json.optJSONObject("notes"));
		addJSONAnnotations(json.optJSONObject("annotation"));
	}

	private void addJSONNotes(JSONObject jnotes) {
		if (jnotes == null || jnotes.isEmpty()) {
			return;
		}
		// FIXME: extract JSON notes
		System.out.println("##### TODO: ADD JSON NOTES");
	}

	private void addJSONAnnotations(JSONObject jannot) {
		if (jannot == null || jannot.isEmpty()) {
			return;
		}

		for (String qualifier : jannot.keySet()) {
			JSONArray blocks = jannot.getJSONArray(qualifier);
			if (blocks == null || blocks.isEmpty()) {
				continue;
			}
			int len = blocks.length();
			for (int i=0 ; i<len ; i++) {
				qualify(qualifier, -1);
				JSONObject alt = blocks.getJSONObject(i);

				JSONArray tags = alt.optJSONArray("tags");
				if (tags != null) {
					for (Object t: tags) {
						tag(t.toString());
					}
				}

				JSONObject kv = alt.optJSONObject("keysvalues");
				if (kv != null) {
					for (String k: kv.keySet()) {
						String v = kv.getString(k);
						this.put(k, v);
					}
				}
			}
		}
	}


	/**
	 * Import a structured json file to populate the metadata of the model
	 * 
	 * @param filename the name of the json file
	 * @param nodes
	 */
	public void importMetadata(String filename, Iterable<N> nodes) {
		Map<String, N> nodemap = new HashMap<>();
		nodes.forEach(ni -> nodemap.put(ni.toString(), ni));
		importMetadata(filename, nodemap);
	}

	public void importMetadata(String filename, Map<String, N> nodeMap) {
		try {
			// we load the json file
			JSONObject json = JsonReader.readJsonFromFile(filename);
			this.readAnnotationsFromJSON(json, nodeMap);
		} catch (IOException e) {
			e.printStackTrace();
		}// TODO Auto-generated catch block

	}

	public boolean isModel() {
		return this.selected == null;
	}

	public boolean hasData() {
		Metadata mdt = this.getMetadata();
		return mdt != null && !mdt.isEmpty();
	}
}
