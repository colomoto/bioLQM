package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.JsonReader;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.json.JSONArray;
import org.json.JSONObject;
import org.colomoto.biolqm.metadata.constants.Index;
import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.NodeInfo;

import java.util.Map;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;

/**
 * One instance per model opened containing all the elements relative to the annotation process
 *
 * @author Martin Boutroux
 */
public class AnnotationModule {
	
	// variables
	private ModelConstants modelConstants;
	
	private Index modelIndex;
	private Map<NodeInfo, Index> nodesIndex;
	private Map<NodeInfoPair, Index> edgesIndex;
	
	// constructors
	public AnnotationModule() throws Exception {
		this.modelConstants = new ModelConstants();
		
		Metadata modelMetadata = new Metadata(this.modelConstants, "model", false);
		this.modelIndex = new Index(this.modelConstants.getIncrement());
		this.modelConstants.getListMetadata().put(modelIndex, modelMetadata);
		
		this.nodesIndex = new HashMap<NodeInfo, Index>();
		this.edgesIndex = new HashMap<NodeInfoPair, Index>();
	}
	
	// getters
	public ModelConstants getModelConstants() {
		return this.modelConstants;
	}
	public Index getModelIndex() {
		return this.modelIndex;
	}
	public Map<NodeInfo, Index> getNodesIndex() {
		return this.nodesIndex;
	}
	public Map<NodeInfoPair, Index> getEdgesIndex() {
		return this.edgesIndex;
	}
	
	// setters
	public void setNodesIndex(Map<NodeInfo, Index> newNodesIndex) {
		this.nodesIndex = newNodesIndex;
	}
	public void setEdgesIndex(Map<NodeInfoPair, Index> newEdgesIndex) {
		this.edgesIndex = newEdgesIndex;
	}
	
	// functions
	/**
	 * Create a Metadata object for a node of the model
	 *
	 * @param node the node you want to annotate
	 * @return the Metadata object you created for the node
	 * @throws Exception 
	 */
	public Metadata createMetadataOfNode(NodeInfo node) throws Exception {
		
		Metadata nodeMetadata = new Metadata(this.modelConstants, "species", false);
		Index nodeIndex = new Index(this.modelConstants.getIncrement());
		this.modelConstants.getListMetadata().put(nodeIndex, nodeMetadata);
		
		this.nodesIndex.put(node, nodeIndex);
		
		return nodeMetadata;
	}
	
	/**
	 * Create a Metadata object for an edge of the model
	 *
	 * @param edge the edge you want to annotate
	 * @return the Metadata object you created for the edge
	 * @throws Exception 
	 */
	public Metadata createMetadataOfEdge(NodeInfoPair edge) throws Exception {
		
		Metadata edgeMetadata = new Metadata(this.modelConstants, "transition", false);
		Index edgeIndex = new Index(this.modelConstants.getIncrement());
		this.modelConstants.getListMetadata().put(edgeIndex, edgeMetadata);
		
		this.edgesIndex.put(edge, edgeIndex);
		
		return edgeMetadata;
	}
	
	/**
	 * Retrieve the Metadata object of the model
	 * @return the existing Metadata of the model
	 */
	public Metadata getMetadataOfModel() {
		
		return this.modelConstants.getListMetadata().get(this.modelIndex);
	}
	
	/**
	 * Check if a metadata object exists for a node
	 *
	 * @param node the node you want to check
	 * @return true if it exists, false otherwise
	 */	
	public boolean isSetMetadataOfNode(NodeInfo node) {
		if (this.nodesIndex.containsKey(node)) {
			return true;
		}
		return false;
	}
	
	/**
	 * Check if a metadata object exists for an edge
	 *
	 * @param edge the edge you want to check
	 * @return true if it exists, false otherwise
	 */	
	public boolean isSetMetadataOfEdge(NodeInfoPair edge) {
		
		if (this.edgesIndex.containsKey(edge)) {
			return true;
		}
		return false;
	}
	
	/**
	 * Retrieve the Metadata object of the node
	 * 
	 * @param node the node you want to annotate
	 * @return the existing Metadata of the node. Create it if it does not exist.
	 * @throws Exception 
	 */
	public Metadata getMetadataOfNode(NodeInfo node) throws Exception {
		
		try {
			if (this.nodesIndex.containsKey(node)) {
				return this.modelConstants.getListMetadata().get(this.nodesIndex.get(node));
			}
			else {
				return this.createMetadataOfNode(node);
			}
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Retrieve the Metadata object of an edge
	 * 
	 * @param edge the edge you want to annotate
	 * @return the existing Metadata of the node. Create it if it does not exist.
	 * @throws Exception 
	 */
	public Metadata getMetadataOfEdge(NodeInfoPair edge) throws Exception {
		try {
			if (this.edgesIndex.containsKey(edge)) {
				return this.modelConstants.getListMetadata().get(this.edgesIndex.get(edge));
			}
			else {
				return this.createMetadataOfEdge(edge);
			}
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Retrieve the Metadata object of an edge
	 * 
	 * @param node1 the source node of the edge
	 * @param node2 the target node of the edge
	 * @return the existing Metadata of the node. Create it if it does not exist.
	 * @throws Exception 
	 */
	public Metadata getMetadataOfEdge(NodeInfo node1, NodeInfo node2) throws Exception {
		
		NodeInfoPair edge = new NodeInfoPair(node1, node2);
		return this.getMetadataOfEdge(edge);
	}
	
	private void exportElementMetadata(Metadata metadata, JSONObject json) {
		
		// if there is some metadata we add the json representation in the json object
		if (metadata.isMetadataNotEmpty()) {
			
			json.put("annotation", metadata.getJSONOfMetadata());
		}
		// if there is some notes we add the json representation in the json object
		if (metadata.getNotes() != "") {
			
			json.put("notes", metadata.getNotes());
		}
	}
	
	/**
	 * Write the json of all the annotations in the model
	 * 
	 * @param coreNodes
	 * @param extraNodes
	 * 
	 */
	public JSONObject writeAnnotationsInJSON(List<NodeInfo> coreNodes, List<NodeInfo> extraNodes, ConnectivityMatrix matrix) {
		JSONObject json = new JSONObject();
		
		Metadata metadataModel = this.getMetadataOfModel();
		
		metadataModel.exportCollectionsMetadata(json);
		
		if (metadataModel.isMetadataNotEmpty() || metadataModel.getNotes() != "") {
			this.exportElementMetadata(metadataModel, json);
		}
		
		JSONArray jsonArrayNodes = new JSONArray();
		
		for (NodeInfo node: nodesIndex.keySet()) {
			
			if (this.isSetMetadataOfNode(node)) {
				Metadata metadataSpecies;
				try {
					metadataSpecies = this.getMetadataOfNode(node);
					
					if (metadataSpecies.isMetadataNotEmpty() || metadataSpecies.getNotes() != "") {
						JSONObject jsonNode = new JSONObject();
						
						String nodeId = node.getNodeID();
						jsonNode.put("id", nodeId);
						exportElementMetadata(metadataSpecies, jsonNode);
						
						jsonArrayNodes.put(jsonNode);
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		
		json.put("nodes", jsonArrayNodes);
		
		JSONArray jsonArrayEdges = new JSONArray();
			
		for (NodeInfoPair edge: edgesIndex.keySet()) {
			
			boolean edgeExists = false;
			
			NodeInfo node1 = edge.getNode1();
			NodeInfo node2 = edge.getNode2();
			
			int idx1 = -1;
			int idx2 = -1;
			boolean extra2 = false;
			if (coreNodes.contains(node1)) {
				idx1 = coreNodes.indexOf(node1);
			}
			if (coreNodes.contains(node2)) {
				idx2 = coreNodes.indexOf(node2);
			} else if (extraNodes.contains(node2)) {
				idx2 = extraNodes.indexOf(node2);
				extra2 = true;
			}
			
			if (idx1 != -1 && idx2 != -1) {
				for(int y: matrix.getRegulators(idx2, extra2)){
				    if(y == idx1){
				    	edgeExists = true;
				        break;
				    }
				}
			}
			
			if (edgeExists && this.isSetMetadataOfEdge(edge)) {
				Metadata metadataEdge;
				try {
					metadataEdge = this.getMetadataOfEdge(edge);
					
					if (metadataEdge.isMetadataNotEmpty() || metadataEdge.getNotes() != "") {
						JSONObject jsonEdge = new JSONObject();
						
						String nodeId1 = edge.getNode1().getNodeID();
						String nodeId2 = edge.getNode2().getNodeID();
						jsonEdge.put("id1", nodeId1);
						jsonEdge.put("id2", nodeId2);
						exportElementMetadata(metadataEdge, jsonEdge);
						
						jsonArrayEdges.put(jsonEdge);
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		
		json.put("edges", jsonArrayEdges);
		
		return json;
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
			
			for(int idQualifier = 0; idQualifier < arrayQualifiers.length(); idQualifier++)
			{
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
	 * 
	 */
	public void readAnnotationsFromJSON(JSONObject json, List<NodeInfo> coreNodes, List<NodeInfo> extraNodes) {
		Metadata metadataModel = this.getMetadataOfModel();
		
		if (json.has("collections") && !json.isNull("collections")) {	 
			
			metadataModel.importCollectionsMetadata(json.getJSONArray("collections"));
		}
		
		// we import the metadata concerning the model
		if ((json.has("annotation") && !json.isNull("annotation")) || (json.has("notes") && !json.isNull("notes"))) {	 
			metadataModel.importElementMetadata(json);
		}
		
		// we import the metadata concerning each node
		if (json.has("nodes") && !json.isNull("nodes")) {
			JSONArray arrayNodes = json.getJSONArray("nodes");
			for(int idNode = 0; idNode < arrayNodes.length(); idNode++)
			{
				JSONObject jsonNode = arrayNodes.getJSONObject(idNode);
				String nodeId = jsonNode.getString("id");

				NodeInfo node = null;
				if (coreNodes != null) {
					for (NodeInfo elmt: coreNodes) {
						if (elmt.getNodeID().equals(nodeId)) {
							node = elmt;
						}
					}
				}
				if (extraNodes != null) {
					for (NodeInfo elmt: extraNodes) {
						if (elmt.getNodeID().equals(nodeId)) {
							node = elmt;
						}
					}
				}
				
				if (node != null) {
					try {
						Metadata metadataNode = this.getMetadataOfNode(node);
						metadataNode.importElementMetadata(jsonNode);	
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				} else {
					System.err.println("The node "+jsonNode.getString("id")+" has no equivalent in the model so its annotations couldn't be imported.");
				}
			}
		}
		
		// we import the metadata concerning each edge
		if (json.has("edges") && !json.isNull("edges")) {
			JSONArray arrayEdges = json.getJSONArray("edges");
			for(int idEdge = 0; idEdge < arrayEdges.length(); idEdge++)
			{
				JSONObject jsonEdge = arrayEdges.getJSONObject(idEdge);
				String nodeId1 = jsonEdge.getString("id1");
				String nodeId2 = jsonEdge.getString("id2");

				NodeInfo node1 = null;
				NodeInfo node2 = null;
				if (coreNodes != null) {
					for (NodeInfo elmt: coreNodes) {
						if (elmt.getNodeID().equals(nodeId1)) {
							node1 = elmt;
						}
						if (elmt.getNodeID().equals(nodeId2)) {
							node2 = elmt;
						}
					}
				}
				if (extraNodes != null) {
					for (NodeInfo elmt: extraNodes) {
						if (elmt.getNodeID().equals(nodeId1)) {
							node1 = elmt;
						}
						if (elmt.getNodeID().equals(nodeId2)) {
							node2 = elmt;
						}
					}
				}
				
				if (node1 != null && node2 != null) {
					try {
						Metadata metadataEdge = this.getMetadataOfEdge(node1, node2);
						metadataEdge.importElementMetadata(jsonEdge);	
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				} else {
					System.err.println("The edge ("+jsonEdge.getString("id1")+", "+jsonEdge.getString("id2")+") has no equivalent in the model so its annotations couldn't be imported.");
				}
			}
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
			
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}
}