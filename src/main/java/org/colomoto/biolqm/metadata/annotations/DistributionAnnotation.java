package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
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
	protected boolean addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) {

		if (!this.distribution.equals("")) {
			this.distribution = contentAnnotation[0];
			return false;
		}
		this.distribution = contentAnnotation[0];
		return true;
	}
	
	@Override
	protected void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		
	}
	
	@Override
	protected String getValue(String tab) {
		
		String chaine = ":\n";
		
		chaine += tab + "\t" + "Terms of distribution : " + this.distribution + "\n";
		
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
	protected Index getIndex(ModelConstants modelConstants, Index indexParent) throws Exception {

		Index existingIndex;
		
		if (this.indexOfDistribution != null) {
			existingIndex = this.indexOfDistribution;
		}
		else {
			existingIndex = new Index(indexParent, modelConstants.getIncrement());
			
			indexParent.setIndexOfChildren(existingIndex);
			
			this.indexOfDistribution = existingIndex;
			
			// we get the type of the parent Metadata
			String type = modelConstants.getListMetadata().get(indexParent).getType();
			
			modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, type, true));
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
	protected boolean doesAlternativeExist(JSONObject jsonAlternative) {
		
		if (this.distribution.equals(jsonAlternative.getString("distribution"))) {
			return true;
		}
		return false;
	}
	
	@Override
	protected String getShortDescription() {
		return "1 distribution";
	}
	
	@Override
	public boolean isNotEmpty() {
		return false;
	}
	
	@Override
	public boolean sameAnnotation(Object obj) {
		DistributionAnnotation dist = (DistributionAnnotation) obj;
		if (!dist.getDistribution().equals(this.getDistribution())) {
			return false;
		}
		return true;
	}
}