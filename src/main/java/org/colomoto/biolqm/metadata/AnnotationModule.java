package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.*;

import org.colomoto.biolqm.metadata.constants.Collection;
import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.colomoto.biolqm.metadata.validations.PatternValidator;
import org.json.JSONArray;
import org.json.JSONObject;
import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.NodeInfo;

import java.util.Map;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;

/**
 * One instance per model containing all the elements relative to the annotation process
 *
 * @author Martin Boutroux
 * @author Aurelien Naldi
 */
public class AnnotationModule {

	/** Keep track of available qualifiers, collections, tags, ... */
	private final ModelConstants modelConstants = new ModelConstants();

	/** Assign metadata to the model itself */
	private Metadata modelMetadata = null;

	/** Assign metadata to each annotated element */
	private final Map<Object, Metadata> annotations = new HashMap<>();

	private LegalAnnotation legal = null;

	private AnnotationTarget target = AnnotationTarget.Model;
	private Object selected = null;
	private Qualifier qualifier = null;
	private int alternative = 0;

	/**
	 * Check if any element is annotated in this model
	 * @return true if no annotations exist on the model itself or any of its elements
	 */
	public boolean isEmpty() {
		return this.legal == null && this.modelMetadata == null && this.annotations.isEmpty();
	}

	public LegalAnnotation getLegal() {
		return this.legal;
	}
	public LegalAnnotation ensureLegal() {
		if (this.legal == null) {
			this.legal = new LegalAnnotation();
		}
		return this.legal;
	}

	/**
	 * Retrieve the annotations associated to the current selection.
	 * @return the existing annotation or null if it is not defined.
	 */
	private Metadata getAnnotation() {
		switch (this.target) {
			case Model:
				return this.modelMetadata;
			case Component:
			case Interaction:
				return this.annotations.get(this.selected);
			default:
				System.err.println("Should be unreachable");
				return null;
		}
	}

	/**
	 * Retrieve or create the annotations associated to the current selection.
	 * @return the existing annotation or a new one if it was not defined.
	 */
	private Metadata ensureMetadata() {
		switch (this.target) {
			case Model:
				if (this.modelMetadata == null) {
					this.modelMetadata = new Metadata();
				}
				return this.modelMetadata;
			case Component:
			case Interaction:
				Metadata meta = this.annotations.get(this.selected);
				if (meta == null) {
					meta = new Metadata();
					this.annotations.put(this.selected, meta);
				}
				return meta;
			default:
				System.err.println("Should be unreachable");
				return null;
		}
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

	public void selectModel() {
		setSelection(AnnotationTarget.Model, null);
	}

	public void selectNode(NodeInfo node) {
		if (node == null) {
			selectModel();
			return;
		}
		this.setSelection(AnnotationTarget.Component, node);
	}

	public void selectEdge(NodeInfoPair edge) {
		if (edge == null) {
			selectModel();
			return;
		}
		this.setSelection(AnnotationTarget.Interaction, edge);
	}

	public void selectQualifier(String qualifier) {
		this.selectQualifier(qualifier, 0);
	}
	public void selectQualifier(String qualifier, int alternative) {
		this.alternative = alternative;
		if (qualifier == null) {
			this.qualifier = null;
			return;
		}
		this.qualifier = this.modelConstants.getInstanceOfQualifiersAvailable().ensureQualifier(this.target, qualifier);
	}

	/**
	 * Guess the annotation type and add it
	 *
	 * @param s a string representing the annotation
	 */
	public void annotate(String s) {
		// FIXME: guess annotation type and add it in the right spot
		Matcher m = PatternValidator.matchTag(s);
		if (m.matches()) {
			this.addTag(m.group(0));
			return;
		}

		m = PatternValidator.matchCollection(s);
		if (m.matches()) {
			this.addCollectionEntry(m.group(0), m.group(1));
		}
	}

	// Fill annotations
	public void addTag(String tag) {
		Annotation annot = this.ensureAnnotation();
		if (annot.tags.add(tag)) {
			this.modelConstants.addTag(tag);
		}
	}

	public void addKeyValue(String key, String value) {
		Annotation annot = this.ensureAnnotation();
		if (annot.addKeyValue(key, value)) {
			this.modelConstants.addKey(key);
		}
	}

	public void addCollectionEntry(String col, String entry) {
		Annotation annot = this.ensureAnnotation();
		Collection collection = modelConstants.getInstanceOfCollectionsAvailable().getCollection(col);
		URI uri = new URI(collection, entry);
		if (annot.uris.add(uri)) {
//			this.modelConstants.(key);
		}
	}

	/**
	 * Write the JSON of all the annotations in the model
	 */
	public JSONObject writeAnnotationsInJSON(List<NodeInfo> coreNodes, List<NodeInfo> extraNodes, ConnectivityMatrix matrix) {
		JSONObject json = new JSONObject();

		if (this.modelMetadata != null) {
			// FIXME: export collection metadata
			// this.modelMetadata.exportCollectionsMetadata(json);
			this.modelMetadata.toJSON(json);
		}

		// Store all annotations on nodes and edges
		JSONArray jsonArrayNodes = new JSONArray();
		JSONArray jsonArrayEdges = new JSONArray();

		// Fill annotations
		int idx = 0;
		for (NodeInfo ni: coreNodes) {
			this.fillJSONForNode(ni, jsonArrayNodes);
			int[] regulators = matrix.getRegulators(idx, false);
			idx++;
			for (int reg: regulators) {
				NodeInfoPair nip = new NodeInfoPair(coreNodes.get(reg), ni);
				fillJSONForEdge(nip, jsonArrayEdges);
			}
		}

		idx = 0;
		for (NodeInfo ni: extraNodes) {
			this.fillJSONForNode(ni, jsonArrayNodes);
			int[] regulators = matrix.getRegulators(idx, true);
			idx++;
			for (int reg: regulators) {
				NodeInfoPair nip = new NodeInfoPair(coreNodes.get(reg), ni);
				fillJSONForEdge(nip, jsonArrayEdges);
			}
		}

		json.put("nodes", jsonArrayNodes);
		json.put("edges", jsonArrayEdges);

		return json;
	}

	private void fillJSONForNode(NodeInfo ni, JSONArray jsonArrayNodes) {
		this.selectNode(ni);
		Metadata meta = this.getAnnotation();
		if (meta == null || meta.isEmpty()) {
			return;
		}
		JSONObject jsonNode = new JSONObject();
		jsonNode.put("id", ni.getNodeID());
		meta.toJSON(jsonNode);
		jsonArrayNodes.put(jsonNode);
	}

	private void fillJSONForEdge(NodeInfoPair nip, JSONArray jsonArrayEdges) {
		this.selectEdge(nip);
		Metadata meta = this.getAnnotation();
		if (meta == null || meta.isEmpty()) {
			return;
		}
		JSONObject jsonEdge = new JSONObject();
		jsonEdge.put("id1", nip.getNode1().getNodeID());
		jsonEdge.put("id2", nip.getNode2().getNodeID());
		meta.toJSON(jsonEdge);
		jsonArrayEdges.put(jsonEdge);
	}
	
	/**
	 * Export all the metadata of the model in a structured json file
	 * 
	 * @param filename the name of the json file
	 * @param coreNodes
	 * @param extraNodes
	 * 
	 */
	public void exportMetadata(String filename, List<NodeInfo> coreNodes, List<NodeInfo> extraNodes, ConnectivityMatrix matrix) {
		
		JSONObject json = writeAnnotationsInJSON(coreNodes, extraNodes, matrix);
		
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
	 * @param coreNodes
	 * @param extraNodes
	 */
	public void readAnnotationsFromJSON(JSONObject json, List<NodeInfo> coreNodes, List<NodeInfo> extraNodes) {
		// FIXME: import collection metadata
//		if (json.has("collections") && !json.isNull("collections")) {
//			metadataModel.importCollectionsMetadata(json.getJSONArray("collections"));
//		}

		this.selectModel();
		this.addJSON(json);

		// Map all known IDs to NodeInfo objects
		Map<String, NodeInfo> nodeMap = new HashMap<>();
		for (NodeInfo ni: coreNodes) {
			nodeMap.put(ni.getNodeID(), ni);
		}
		for (NodeInfo ni: extraNodes) {
			nodeMap.put(ni.getNodeID(), ni);
		}

		// Import the metadata for each node
		if (json.has("nodes") && !json.isNull("nodes")) {
			JSONArray arrayNodes = json.getJSONArray("nodes");
			for(int idNode = 0; idNode < arrayNodes.length(); idNode++) {
				JSONObject jsonNode = arrayNodes.getJSONObject(idNode);
				String nodeId = jsonNode.getString("id");
				NodeInfo ni = nodeMap.get(nodeId);
				if (ni == null) {
					System.err.println("The node "+nodeId+" has no equivalent in the model so its annotations couldn't be imported.");
					continue;
				}

				this.selectNode(ni);
				this.addJSON(jsonNode);
			}
		}
		
		// we import the metadata concerning each edge
		if (json.has("edges") && !json.isNull("edges")) {
			JSONArray arrayEdges = json.getJSONArray("edges");
			for(int idEdge = 0; idEdge < arrayEdges.length(); idEdge++) {
				JSONObject jsonEdge = arrayEdges.getJSONObject(idEdge);
				NodeInfo ni1 = nodeMap.get(jsonEdge.getString("id1"));
				NodeInfo ni2 = nodeMap.get(jsonEdge.getString("id2"));

				if (ni1 == null || ni2 == null) {
					System.err.println("The edge ("+jsonEdge.getString("id1")+", "+jsonEdge.getString("id2")+") has no equivalent in the model so its annotations couldn't be imported.");
					continue;
				}

				this.selectEdge(new NodeInfoPair(ni1, ni2));
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
	 * @param coreNodes
	 * @param extraNodes
	 * 
	 */
	public void importMetadata(String filename, List<NodeInfo> coreNodes, List<NodeInfo> extraNodes) {
		try {
			// we load the json file
			JSONObject json = JsonReader.readJsonFromFile(filename);
			
			this.readAnnotationsFromJSON(json, coreNodes, extraNodes);
			
		} catch (IOException e) {
			e.printStackTrace();
		}// TODO Auto-generated catch block

	}
}