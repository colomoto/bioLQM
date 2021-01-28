package org.colomoto.biolqm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

import org.json.JSONObject;
import org.json.JSONArray;
import org.json.JSONTokener;

import org.colomoto.mddlib.MDDManager;

import org.colomoto.biolqm.metadata.AnnotationModule;
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
		
		this.annotationModule = new AnnotationModule();
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
	public boolean isSetMetadataOfNode(NodeInfo node) {
		if (this.annotationModule.nodesIndex.containsKey(node)) {
			return true;
		}
		return false;
	}
	
	@Override
	public Metadata getMetadataOfNode(NodeInfo node) {
		
		if (this.annotationModule.nodesIndex.containsKey(node)) {
			return this.annotationModule.modelConstants.getListMetadata().get(this.annotationModule.nodesIndex.get(node));
		}
		else {
			return this.annotationModule.createMetadataOfNode(node);
		}
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
	
	@Override
	public void exportMetadata(String filename) {
		
		JSONObject json = new JSONObject();
		
		Metadata metadataModel = this.getMetadataOfModel();
		
		if (metadataModel.isMetadataNotEmpty() || metadataModel.getNotes() != "") {
			this.exportElementMetadata(metadataModel, json);
		}
		
		JSONArray jsonArray = new JSONArray();
		
		for (NodeInfo node: this.coreNodes) {
			
			if (this.isSetMetadataOfNode(node)) {
				Metadata metadataSpecies = this.getMetadataOfNode(node);
				
				if (metadataSpecies.isMetadataNotEmpty() || metadataSpecies.getNotes() != "") {
					JSONObject jsonNode = new JSONObject();
					
					jsonNode.put("id", node.getNodeID());
					exportElementMetadata(metadataSpecies, jsonNode);
					
					jsonArray.put(jsonNode);
				}
			}
		}
		
		for (NodeInfo node: this.extraNodes) {
			
			if (this.isSetMetadataOfNode(node)) {
				Metadata metadataSpecies = this.getMetadataOfNode(node);
				
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
        try (FileWriter file = new FileWriter(filename+".json")) {
 
            file.write(json.toString());
            file.flush();
 
        } catch (IOException e) {
            e.printStackTrace();
        }
	}
	
	private void importElementMetadata(Metadata metadata, JSONObject json) {
		
		// if there is some metadata we add the json representation in the json object
		if (json.has("annotation") && !json.isNull("annotation")) {
			
			JSONArray arrayQualifiers = json.getJSONArray("annotation");
			for(int idQualifier = 0; idQualifier < arrayQualifiers.length(); idQualifier++)
			{
				JSONObject jsonQualifier = arrayQualifiers.getJSONObject(idQualifier);
				
				String qualifierName = jsonQualifier.getString("qualifier");
				String qualifierClass = jsonQualifier.getString("type");
				
				JSONArray arrayAlternatives = jsonQualifier.getJSONArray("alternatives");

				if (qualifierClass.equals("GenericAnnotation")) {
					
					int numberAlternative = metadata.getNumberOfAlternatives(qualifierName);
					for(int idAlternative = 0; idAlternative < arrayAlternatives.length(); idAlternative++)
					{
						JSONObject jsonAlternative = arrayAlternatives.getJSONObject(idAlternative);
						if (numberAlternative > 0) {
							metadata.createAlternative(qualifierName);
						}
						
						if (jsonAlternative.has("uris") && !jsonAlternative.isNull("uris")) {
							JSONArray arrayURIs = jsonAlternative.getJSONArray("uris");
							for(int idUri = 0; idUri < arrayURIs.length(); idUri++)
							{
								JSONObject jsonURI = arrayURIs.getJSONObject(idUri);
								metadata.addURI(qualifierName, numberAlternative, jsonURI.getString("collection"), jsonURI.getString("identifier"));
							}
						}
						if (jsonAlternative.has("tags") && !jsonAlternative.isNull("tags")) {
							JSONArray arrayTags = jsonAlternative.getJSONArray("tags");
							for(int idTag = 0; idTag < arrayTags.length(); idTag++)
							{
								String tag = arrayTags.getString(idTag);
								metadata.addTag(qualifierName, numberAlternative, tag);
							}
						}
						if (jsonAlternative.has("keysvalues") && !jsonAlternative.isNull("keysvalues")) {
							JSONArray arrayKeys = jsonAlternative.getJSONArray("keysvalues");
							for(int idKey = 0; idKey < arrayKeys.length(); idKey++)
							{
								JSONObject key = arrayKeys.getJSONObject(idKey);
								JSONArray arrayValues = key.getJSONArray("values");
								
								for (int idValue = 0; idValue < arrayValues.length(); idValue++) {
									metadata.addKeyValue(qualifierName, numberAlternative, key.getString("key"), arrayValues.getString(idValue));
								}
							}
						}
							
						if (jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) {
							
							Metadata metadataQualifier = metadata.getMetadataOfQualifier(qualifierName, numberAlternative);
							
							this.importElementMetadata(metadataQualifier, jsonAlternative.getJSONObject("nested"));
						}
						
						numberAlternative += 1;
					}
				}
				else {
					JSONObject jsonAlternative = arrayAlternatives.getJSONObject(0);
					
					if (qualifierClass.equals("AuthorsAnnotation")) {
						
						JSONArray arrayAuthors = jsonAlternative.getJSONArray("authors");
						for(int idAuthor = 0; idAuthor < arrayAuthors.length(); idAuthor++)
						{
							JSONObject author = arrayAuthors.getJSONObject(idAuthor);
							
							String email = null;
							if (author.has("email") && !author.isNull("email")) { email = author.getString("email"); }
							String organisation = null;
							if (author.has("organisation") && !author.isNull("organisation")) { organisation = author.getString("organisation"); }
							String orcid = null;
							if (author.has("orcid") && !author.isNull("orcid")) { orcid = author.getString("orcid"); }
							
							metadata.addAuthor(qualifierName, author.getString("name"), author.getString("surname"), email, organisation, orcid);
						}
					}
					else if (qualifierClass.equals("DateAnnotation")) {
						
						metadata.addDate(qualifierName, jsonAlternative.getString("date"));
					}
					else if (qualifierClass.equals("DistributionAnnotation")) {

						metadata.addDistribution(qualifierName, jsonAlternative.getString("distribution"));
					}
					
					if (jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) {
						
						Metadata metadataQualifier = metadata.getMetadataOfQualifier(qualifierName);
						
						this.importElementMetadata(metadataQualifier, jsonAlternative.getJSONObject("nested"));
					}
				}
			}
		}
		// if there is some notes we add the json representation in the json object
		if (json.has("notes") && !json.isNull("notes")) {
			
			String existingNotes = metadata.getNotes();
			metadata.setNotes(existingNotes + json.get("notes"));
		}
	}
	
	@Override
	public void importMetadata(String filename) {
		try {
			// we load the json file
			File initialFile = new File(filename+".json");
			InputStream is = new FileInputStream(initialFile);
			JSONTokener tokener = new JSONTokener(is);
			JSONObject json = new JSONObject(tokener);
			
			// we import the metadata concerning the model
			if ((json.has("annotation") && !json.isNull("annotation")) || (json.has("notes") && !json.isNull("notes"))) {	 
				Metadata metadataModel = this.getMetadataOfModel();
				
				this.importElementMetadata(metadataModel, json);
			}
			
			// we import the metadata concerning each node
			JSONArray arrayNodes = json.getJSONArray("nodes");
			for(int idNode = 0; idNode < arrayNodes.length(); idNode++)
			{
				JSONObject jsonNode = arrayNodes.getJSONObject(idNode);
				NodeInfo node = this.getComponent(jsonNode.getString("id"));
				
				if (node != null) {
					Metadata metadataNode = this.getMetadataOfNode(node);
					
					this.importElementMetadata(metadataNode, jsonNode);
				}
			}
			
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
            e.printStackTrace();
        }
	}
}
