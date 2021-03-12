package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.Index;

import org.colomoto.biolqm.NodeInfo;

import java.util.Map;
import java.util.HashMap;

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
}