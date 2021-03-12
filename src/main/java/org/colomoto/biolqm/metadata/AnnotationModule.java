package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.JsonReader;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.colomoto.biolqm.metadata.constants.Index;

import org.colomoto.biolqm.NodeInfo;

import java.util.Map;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;

/**
 * One instance per model opened containing all the elements relative to the annotation process
 *
 * @author Martin Boutroux
 */
public class AnnotationModule {
	
	// variables
	public ModelConstants modelConstants;
	
	public Index modelIndex;
	public Map<String, Index> nodesIndex;
	
	// constructors
	public AnnotationModule() throws Exception {
		this.modelConstants = new ModelConstants();
		
		Metadata modelMetadata = new Metadata(this.modelConstants, "model");
		this.modelIndex = new Index(this.modelConstants.getIncrement());
		this.modelConstants.getListMetadata().put(modelIndex, modelMetadata);
		
		this.nodesIndex = new HashMap<String, Index>();
	}
	
	// functions
	/**
	 * Create a Metadata object for a node of the model
	 *
	 * @param node the node you want to annotate
	 * @return the Metadata object you created for the node
	 * @throws Exception 
	 */
	public Metadata createMetadataOfNode(String nodeId) throws Exception {
		
		Metadata nodeMetadata = new Metadata(this.modelConstants, "species");
		Index nodeIndex = new Index(this.modelConstants.getIncrement());
		this.modelConstants.getListMetadata().put(nodeIndex, nodeMetadata);
		
		this.nodesIndex.put(nodeId, nodeIndex);
		
		return nodeMetadata;
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
	public boolean isSetMetadataOfNode(String nodeId) {
		if (this.nodesIndex.containsKey(nodeId)) {
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
	public Metadata getMetadataOfNode(String nodeId) throws Exception {
		
		try {
			if (this.nodesIndex.containsKey(nodeId)) {
				return this.modelConstants.getListMetadata().get(this.nodesIndex.get(nodeId));
			}
			else {
				return this.createMetadataOfNode(nodeId);
			}
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private void exportElementMetadata(Metadata metadata, JSONObject json) throws JSONException, Exception {
		
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
	 * Export all the metadata of the model in a structured json file
	 * 
	 * @param filename the name of the json file
	 * @throws Exception 
	 * @throws JSONException 
	 */
	public void exportMetadata(String filename) throws JSONException, Exception {
		
		JSONObject json = new JSONObject();
		
		Metadata metadataModel = this.getMetadataOfModel();
		
		metadataModel.exportCollectionsMetadata(json);
		
		if (metadataModel.isMetadataNotEmpty() || metadataModel.getNotes() != "") {
			this.exportElementMetadata(metadataModel, json);
		}
		
		JSONArray jsonArray = new JSONArray();
		
		for (String nodeId: nodesIndex.keySet()) {
			
			if (this.isSetMetadataOfNode(nodeId)) {
				Metadata metadataSpecies = this.getMetadataOfNode(nodeId);
				
				if (metadataSpecies.isMetadataNotEmpty() || metadataSpecies.getNotes() != "") {
					JSONObject jsonNode = new JSONObject();
					
					jsonNode.put("id", nodeId);
					exportElementMetadata(metadataSpecies, jsonNode);
					
					jsonArray.put(jsonNode);
				}
			}
		}
		
		json.put("nodes", jsonArray);
		
        // Write JSON file
        try (Writer file = new OutputStreamWriter(new FileOutputStream(filename+".json"), StandardCharsets.UTF_8)) {
        	
            file.write(json.toString());
            file.flush();
 
        } catch (IOException e) {
            e.printStackTrace();
        }
	}
	
	/**
	 * Import a structured json file to populate the metadata of the model
	 * 
	 * @param filename the name of the json file
	 * @throws Exception 
	 */
	public void importMetadata(String filename) throws Exception {
		try {
			// we load the json file
			JSONObject json = JsonReader.readJsonFromFile(filename);
			
			Metadata metadataModel = this.getMetadataOfModel();
			
			if (json.has("collections") && !json.isNull("collections")) {	 
				
				metadataModel.importCollectionsMetadata(json.getJSONArray("collections"));
			}
			
			// we import the metadata concerning the model
			try {
				if ((json.has("annotation") && !json.isNull("annotation")) || (json.has("notes") && !json.isNull("notes"))) {	 
					
					metadataModel.importElementMetadata(json);
				}
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			// we import the metadata concerning each node
			JSONArray arrayNodes = json.getJSONArray("nodes");
			for(int idNode = 0; idNode < arrayNodes.length(); idNode++)
			{
				JSONObject jsonNode = arrayNodes.getJSONObject(idNode);
				String nodeId = jsonNode.getString("id");
				
				if (nodeId != null) {
					try {
						Metadata metadataNode = this.getMetadataOfNode(nodeId);
						metadataNode.importElementMetadata(jsonNode);	
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				} else {
					System.err.println("The node "+jsonNode.getString("id")+" has no equivalent in the model so its annotations couldn't be imported.");
				}
			}
			
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
}