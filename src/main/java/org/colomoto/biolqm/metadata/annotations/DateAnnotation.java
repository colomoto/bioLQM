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
 * This contains the annotations relative to the date of creation of the model and the last date of modification
 *
 * @author Martin Boutroux
 */
class DateAnnotation extends Annotation {
	
	// variables
	private String date;
	
	private Index indexOfDate;
	
	// constructors
	protected DateAnnotation() {
		this.date = "";
		
		this.indexOfDate = null;
	}
	
	// getters
	protected String getDate() {
		return this.date;
	}
	
	// functions	
	@Override
	protected void addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) {

		this.date = contentAnnotation[0];
	}
	
	@Override
	protected void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		
	}
	
	@Override
	protected String getValue(String tab) {
		
		String chaine = ":\n";
		
		chaine += tab + "\t" + "Date : " + this.date + "\n";
		
		return chaine;
	}
	
	@Override
	protected boolean isSetIndex(ModelConstants modelConstants, Index indexParent) {
		if (this.indexOfDate != null) {
			return true;
		}
		return false;
	}
	
	@Override
	protected Index getIndex(ModelConstants modelConstants, Index indexParent) {

		Index existingIndex;
		
		if (this.indexOfDate != null) {
			existingIndex = this.indexOfDate;
		}
		else {
			existingIndex = new Index(indexParent, modelConstants.getIncrement());
			
			indexParent.setIndexOfChildren(existingIndex);
			
			this.indexOfDate = existingIndex;
			modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
		}
		
		return existingIndex;
	}
	
	@Override
	protected ArrayList<ArrayList<String>> getResources() {
		ArrayList<ArrayList<String>> resources = new ArrayList<ArrayList<String>>();
		ArrayList<String> resource = new ArrayList<String>(Arrays.asList(date));
		resources.add(resource);
		return resources;
	}
	
	@Override
	protected JSONObject getJSONOfAnnotation() {
		JSONObject json = new JSONObject();
		
		json.put("date", this.date);
		
		return json;
	}
	
	@Override
	protected boolean doesAlternativeExist(JSONObject jsonAlternative) {
		
		if (this.date.equals(jsonAlternative.getString("date"))) {
			return true;
		}
		return false;
	}
}