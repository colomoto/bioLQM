package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.Index;

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
	//public Map<Object, Index> elementsIndex;
	
	// constructors
	public AnnotationModule() {
		this.modelConstants = new ModelConstants();
		
		Metadata modelMetadata = new Metadata(this.modelConstants, "model");
		this.modelIndex = new Index(this.modelConstants.getIncrement());
		this.modelConstants.getListMetadata().put(modelIndex, modelMetadata);
		
		//this.elementsIndex = new HashMap<Object, Index>();
	}
}