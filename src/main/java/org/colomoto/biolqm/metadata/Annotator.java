package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.*;
import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.colomoto.biolqm.metadata.validations.PatternValidator;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.Map;
import java.util.regex.Matcher;


/**
 * One instance per model containing all the elements relative to the annotation process
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
	 * Retrieve the full metadata associated to the current selection.
	 * @return the existing metadata or null if it is not defined.
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
		Metadata meta = this.ensureMetadata();
		return meta.ensureAnnotation(this.qualifier, this.alternative);
	}

	private void setSelection(AnnotationTarget target, Object selected) {
		this.target = target;
		this.selected = selected;
		this.qualifier = null;
		this.alternative = 0;
	}

	public Annotator<N> onModel() {
		setSelection(AnnotationTarget.Model, null);
		return this;
	}

/*
	private N findNode(String id) {
		if (id == null) {
			return null;
		}
		return Stream.concat(this.model.getComponents().stream(), this.model.getExtraComponents().stream())
				.filter(ni -> id.equals(ni.getNodeID())).findFirst().orElse(null);
	}

	public Annotator selectNode(String id) {
		return this.selectNode(this.findNode(id));
	}

	public Annotator selectEdge(String src, String tgt) {
		return this.selectEdge(this.findNode(src), this.findNode(tgt));
	}
*/

	public Annotator<N> node(N node) {
		if (node == null) {
			onModel();
			return this;
		}
		this.setSelection(AnnotationTarget.Component, node);
		return this;
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
		this.setSelection(AnnotationTarget.Interaction, edge);
		return this;
	}

	public Annotator<N> qualify(String qualifier) {
		this.qualify(qualifier, 0);
		return this;
	}

	public Annotator<N> qualify(String qualifier, int alternative) {
		this.alternative = alternative;
		if (qualifier == null) {
			this.qualifier = null;
			return this;
		}
		this.qualifier = this.mod.ensureQualifier(this.target, qualifier);
		return this;
	}

	public Annotator<N> nested() {
		// FIXME: implement nested annotations
		return this;
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
			return this.tag(m.group(0));
		}

		m = PatternValidator.matchCollection(s);
		if (m.matches()) {
			return this.identifier(m.group(0), m.group(1));
		}

		// TODO: error
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
	public JSONObject writeAnnotationsInJSON(Iterable<N> nodes, Iterable<Pair<N>> edges) {
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

		// Store all annotations on nodes
		JSONObject jsonNodes = new JSONObject();
		for (N ni: nodes) {
			this.fillJSONForNode(ni, jsonNodes);
		}
		json.put("nodes", jsonNodes);


		// Store all annotations on edges
		JSONObject jsonEdges = new JSONObject();
		for (Pair<N> edge: edges) {
			fillJSONForEdge(edge, jsonEdges);
		}
		json.put("edges", jsonEdges);

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
	 * Export all the metadata of the model in a structured json file
	 * 
	 * @param filename the name of the json file
	 */
	public void exportMetadata(String filename, Iterable<N> nodes, Iterable<Pair<N>> edges) {

		JSONObject json = writeAnnotationsInJSON(nodes, edges);
		
		// we add the date of the day to the modified qualifier before saving
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
		
        // Write JSON file
        try (Writer file = new OutputStreamWriter(new FileOutputStream(filename), StandardCharsets.UTF_8)) {
            file.write(json.toString());
            file.flush();
        } catch (IOException e) {
            e.printStackTrace();
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

		this.onModel();
		this.addJSON(json);

		// Import the metadata for each node
		if (json.has("nodes") && !json.isNull("nodes")) {
			JSONArray arrayNodes = json.getJSONArray("nodes");
			for(int idNode = 0; idNode < arrayNodes.length(); idNode++) {
				JSONObject jsonNode = arrayNodes.getJSONObject(idNode);
				String nodeId = jsonNode.getString("id");
				N ni = nodeMap.get(nodeId);
				if (ni == null) {
					System.err.println("The node "+nodeId+" has no equivalent in the model so its annotations couldn't be imported.");
					continue;
				}

				this.node(ni);
				this.addJSON(jsonNode);
			}
		}
		
		// we import the metadata concerning each edge
		if (json.has("edges") && !json.isNull("edges")) {
			JSONArray arrayEdges = json.getJSONArray("edges");
			for(int idEdge = 0; idEdge < arrayEdges.length(); idEdge++) {
				JSONObject jsonEdge = arrayEdges.getJSONObject(idEdge);
				N ni1 = nodeMap.get(jsonEdge.getString("id1"));
				N ni2 = nodeMap.get(jsonEdge.getString("id2"));

				if (ni1 == null || ni2 == null) {
					System.err.println("The edge ("+jsonEdge.getString("id1")+", "+jsonEdge.getString("id2")+") has no equivalent in the model so its annotations couldn't be imported.");
					continue;
				}

				this.edge(new Pair(ni1, ni2));
				this.addJSON(jsonEdge);
			}
		}
	}

	private void addJSON(JSONObject json) {
		if (json.has("notes") || json.has("annotation")) {
			this.ensureMetadata().fromJSON(json);
		}
	}
	
	/**
	 * Import a structured json file to populate the metadata of the model
	 * 
	 * @param filename the name of the json file
	 * @param nodeMap
	 */
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
