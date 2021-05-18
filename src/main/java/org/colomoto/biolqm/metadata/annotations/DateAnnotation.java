package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
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
	protected boolean addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) {
		
		if (!this.date.equals("")) {
			this.date = contentAnnotation[0];
			return false;
		}
		this.date = contentAnnotation[0];
		return true;
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
	protected Index getIndex(ModelConstants modelConstants, Index indexParent) throws Exception {

		Index existingIndex;
		
		if (this.indexOfDate != null) {
			existingIndex = this.indexOfDate;
		}
		else {
			existingIndex = new Index(indexParent, modelConstants.getIncrement());
			
			indexParent.setIndexOfChildren(existingIndex);
			
			this.indexOfDate = existingIndex;
			
			// we get the type of the parent Metadata
			String type = modelConstants.getListMetadata().get(indexParent).getType();
			
			modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, type, true));
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
	
	@Override
	protected String getShortDescription() {
		return "1 date";
	}
	
	@Override
	public boolean isNotEmpty() {
		return false;
	}
	
	@Override
	public boolean sameAnnotation(Object obj) {
		DateAnnotation date = (DateAnnotation) obj;
		if (!date.getDate().equals(this.getDate())) {
			return false;
		}
		return true;
	}
}