package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.Index;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Special class of annotations
 * This contains the terms of distribution of the model
 *
 * @author Martin Boutroux
 */
class DistributionAnnotation extends Annotation {
	
	// variables
	private String distribution;
	
	private Index indexOfDistribution;
	
	// constructors
	protected DistributionAnnotation() {
		this.distribution = "";
		
		this.indexOfDistribution = null;
	}

	// getters
	protected String getDistribution() {
		return this.distribution;
	}
	
	// functions
	@Override
	protected void addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) {

		this.distribution = contentAnnotation[0];
	}
	
	@Override
	protected void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		
	}
	
	@Override
	protected String getValue(String tab) {
		
		String chaine = ":\n";
		
		chaine += tab + "\t\t" + "Terms of distribution : " + this.distribution + "\n";
		
		return chaine;
	}
	
	@Override
	protected boolean isSetIndex(ModelConstants modelConstants, Index indexParent) {
		if (this.indexOfDistribution != null) {
			return true;
		}
		return false;
	}
	
	@Override
	protected Index getIndex(ModelConstants modelConstants, Index indexParent) {

		Index existingIndex;
		
		if (this.indexOfDistribution != null) {
			existingIndex = this.indexOfDistribution;
		}
		else {
			existingIndex = new Index(indexParent, modelConstants.getIncrement());
			
			indexParent.setIndexOfChildren(existingIndex);
			
			this.indexOfDistribution = existingIndex;
			modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
		}
		
		return existingIndex;
	}

	@Override
	protected ArrayList<ArrayList<String>> getResources() {
		ArrayList<ArrayList<String>> resources = new ArrayList<ArrayList<String>>();
		ArrayList<String> resource = new ArrayList<String>(Arrays.asList(distribution));
		resources.add(resource);
		return resources;
	}
	
	@Override
	protected JSONObject getJSONOfAnnotation() {
		JSONObject json = new JSONObject();
		
		json.put("distribution", this.distribution);
		
		return json;
	}
	
	@Override
	protected boolean isAnnotationNotEmpty() {
		return false;
	}
}