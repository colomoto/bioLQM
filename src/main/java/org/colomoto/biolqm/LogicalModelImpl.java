package org.colomoto.biolqm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import org.json.JSONObject;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONTokener;

import org.colomoto.mddlib.MDDManager;

import org.colomoto.biolqm.metadata.AnnotationModule;
import org.colomoto.biolqm.metadata.annotations.JsonReader;
import org.colomoto.biolqm.metadata.annotations.Metadata;

/**
 * Implementation of the LogicalModel interface.
 * 
 * @author Aurelien Naldi
 */
public class LogicalModelImpl implements LogicalModel {

	private final MDDManager ddmanager;
	
	private final List<NodeInfo> coreNodes;
	private final int[] coreFunctions;
	
	private final List<NodeInfo> extraNodes;
	private final int[] extraFunctions;

	private ModelLayout layout = null;
	
	private AnnotationModule annotationModule;

	public LogicalModelImpl(MDDManager ddmanager, List<NodeInfo> coreNodes, int[] coreFunctions, List<NodeInfo> extraNodes, int[] extraFunctions) {
		this.ddmanager = ddmanager.getManager(coreNodes);
		this.coreNodes = coreNodes;
		this.coreFunctions = coreFunctions;
		
		if (extraNodes == null) {
			this.extraNodes = new ArrayList<>();
			this.extraFunctions = new int[0];
		} else {
			this.extraNodes = extraNodes;
			this.extraFunctions = extraFunctions;
		}
		
		for (int f: this.coreFunctions) {
			this.ddmanager.use(f);
		}
		for (int f: this.extraFunctions) {
			this.ddmanager.use(f);
		}
		
		try {
			this.annotationModule = new AnnotationModule();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public LogicalModelImpl(List<NodeInfo> nodeOrder, MDDManager ddmanager, int[] functions) {
		this(ddmanager, nodeOrder, functions, null, null);
	}

	@Override
	public MDDManager getMDDManager() {
		return ddmanager;
	}

	@Override
	public List<NodeInfo> getComponents() {
		return coreNodes;
	}
	
	@Override
	public int[] getLogicalFunctions() {
		return coreFunctions;
	}

	@Override
	public List<NodeInfo> getExtraComponents() {
		return extraNodes;
	}

	public boolean hasExtraComponents() {
		return extraFunctions != null && extraFunctions.length > 0;
	}

	@Override
	public int[] getExtraLogicalFunctions() {
		return extraFunctions;
	}

	@Override
	public LogicalModel clone() {
		return this.clone(true);
	}

	@Override
	public LogicalModel clone(boolean keepExtra) {
		LogicalModel newModel;
		if (keepExtra) {
			newModel = new LogicalModelImpl(ddmanager, cloneNodes(coreNodes), coreFunctions.clone(), cloneNodes(extraNodes), extraFunctions.clone());
		} else {
			newModel = new LogicalModelImpl(ddmanager, cloneNodes(coreNodes), coreFunctions.clone(), new ArrayList<>(), new int[0]);
		}

		// Transfer the booleanized groups to the cloned model
		cloneBooleanizedInfo(coreNodes);
		if (keepExtra) {
			cloneBooleanizedInfo(extraNodes);
		}

		// Also copy the model layout
		if (this.hasLayout()) {
			ModelLayout mlayout = getLayout();
			ModelLayout newLayout = newModel.getLayout();
			copyLayout(getComponents(), newModel.getComponents(), mlayout, newLayout);
			if (keepExtra) {
				copyLayout(getExtraComponents(), newModel.getExtraComponents(), mlayout, newLayout);
			}
		}
		return newModel;
	}

	private void copyLayout(List<NodeInfo> sourceNodes, List<NodeInfo> targetNodes, ModelLayout sourceLayout, ModelLayout targetLayout) {
		int n = targetNodes.size();
		for (int i=0 ; i<n ; i++) {
			targetLayout.copy(targetNodes.get(i), sourceLayout.getInfo( sourceNodes.get(i)));
		}
	}

	private List<NodeInfo> cloneNodes(List<NodeInfo> source) {
		List<NodeInfo> result = new ArrayList<>(source.size());
		for (NodeInfo ni: source) {
			result.add(ni.clone());
		}
		return result;
	}

	private void cloneBooleanizedInfo(List<NodeInfo> source) {
		for (NodeInfo ni: source) {
			NodeInfo[] grp = ni.getBooleanizedGroup();
			if (grp != null) {
				NodeInfo[] result = new NodeInfo[grp.length];
				NodeInfo target = this.getComponent(ni.getNodeID());
				for (int i=0 ; i< grp.length ; i++) {
					result[i] = this.getComponent(grp[i].getNodeID());
				}
				target.setBooleanizedGroup(result);
			}
		}
	}

	@Override
	public byte getTargetValue(int nodeIdx, byte[] state) {
		return ddmanager.reach(coreFunctions[nodeIdx], state);
	}

	@Override
	public byte getExtraValue(int nodeIdx, byte[] state) {
		return ddmanager.reach(extraFunctions[nodeIdx], state);
	}

	@Override
	public void fillExtraValues(byte[] state, byte[] extra) {
		for (int i=0 ; i<extra.length ; i++) {
			extra[i] = getExtraValue(i, state);
		}
	}

	@Override
	public LogicalModel getView(List<NodeInfo> neworder) {
		
		MDDManager newmanager = ddmanager.getManager(neworder);

		int[] newcorefunctions = new int[coreFunctions.length];
		for (int i=0 ; i<coreFunctions.length ; i++) {
			NodeInfo ni = coreNodes.get(i);
			int newidx = neworder.indexOf(ni);
			
			newcorefunctions[newidx] = coreFunctions[i];
		}
		
		return new LogicalModelImpl(newmanager, neworder, newcorefunctions, extraNodes, extraFunctions);
	}

    @Override
    public boolean isBoolean() {
        for (NodeInfo ni: getComponents()) {
            if (ni.getMax() > 1) {
                return false;
            }
        }
        for (NodeInfo ni: getExtraComponents()) {
            if (ni.getMax() > 1) {
                return false;
            }
        }

        return true;
    }

	@Override
	public NodeInfo getComponent(String id) {
		if (id == null) {
			return null;
		}

		for (NodeInfo ni: getComponents()) {
			if (id.equals(ni.getNodeID())) {
				return ni;
			}
		}
		for (NodeInfo ni: getExtraComponents()) {
			if (id.equals(ni.getNodeID())) {
				return ni;
			}
		}

		return null;
	}

	@Override
	public int getComponentIndex(String id) {
		if (id == null) {
			return -1;
		}

		int idx = 0;
		for (NodeInfo ni: getComponents()) {
			if (id.equals(ni.getNodeID())) {
				return idx;
			}
			idx++;
		}
		for (NodeInfo ni: getExtraComponents()) {
			if (id.equals(ni.getNodeID())) {
				return idx;
			}
			idx++;
		}
		return -1;
	}

	@Override
	public Map<String, NodeInfo[]> getBooleanizedMap() {
		
		Map<String, NodeInfo[]> bmap = null;
		
		for (NodeInfo ni: getComponents()) {
			NodeInfo[] group = ni.getBooleanizedGroup();
			if (group != null && group[0] == ni) {
				if (bmap == null) {
					bmap = new HashMap<>();
				}
				String key = ni.getNodeID();
				if (key.endsWith("_b1")) {
					key = key.substring(0, key.length()-3);
				}
				bmap.put(key, group);
			}
		}
		return bmap;
	}

	@Override
	public boolean hasLayout() {
		return this.layout != null;
	}

	@Override
	public ModelLayout getLayout() {
		if (this.layout == null) {
			this.layout = new ModelLayout();
		}
		return this.layout;
	}
	
	@Override
	public Metadata getMetadataOfModel() {
		
		return this.annotationModule.modelConstants.getListMetadata().get(this.annotationModule.modelIndex);
	}
	
	@Override
	public boolean isSetMetadataOfNode(String nodeId) {
		if (this.annotationModule.nodesIndex.containsKey(nodeId)) {
			return true;
		}
		return false;
	}
	
	@Override
	public Metadata getMetadataOfNode(String nodeId) throws Exception {
		
		try {
			if (this.annotationModule.nodesIndex.containsKey(nodeId)) {
				return this.annotationModule.modelConstants.getListMetadata().get(this.annotationModule.nodesIndex.get(nodeId));
			}
			else {
				return this.annotationModule.createMetadataOfNode(nodeId);
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
	
	@Override
	public void exportMetadata(String filename) throws JSONException, Exception {
		
		JSONObject json = new JSONObject();
		
		Metadata metadataModel = this.getMetadataOfModel();
		
		metadataModel.exportCollectionsMetadata(json);
		
		if (metadataModel.isMetadataNotEmpty() || metadataModel.getNotes() != "") {
			this.exportElementMetadata(metadataModel, json);
		}
		
		JSONArray jsonArray = new JSONArray();
		
		for (NodeInfo node: this.coreNodes) {
			String nodeId = node.getNodeID();
			
			if (this.isSetMetadataOfNode(nodeId)) {
				Metadata metadataSpecies = this.getMetadataOfNode(nodeId);
				
				if (metadataSpecies.isMetadataNotEmpty() || metadataSpecies.getNotes() != "") {
					JSONObject jsonNode = new JSONObject();
					
					jsonNode.put("id", node.getNodeID());
					exportElementMetadata(metadataSpecies, jsonNode);
					
					jsonArray.put(jsonNode);
				}
			}
		}
		
		for (NodeInfo node: this.extraNodes) {
			String nodeId = node.getNodeID();
			
			if (this.isSetMetadataOfNode(nodeId)) {
				Metadata metadataSpecies = this.getMetadataOfNode(nodeId);
				
				if (metadataSpecies.isMetadataNotEmpty() || metadataSpecies.getNotes() != "") {
					JSONObject jsonNode = new JSONObject();
					
					jsonNode.put("id", node.getNodeID());
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
	
	@Override
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

	@Override
	public void setAnnotationModule(AnnotationModule newAnnotationModule) {
		this.annotationModule = newAnnotationModule;
	}

	@Override
	public AnnotationModule getAnnotationModule() {
		return this.annotationModule;
	}
}
