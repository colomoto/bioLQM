package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.Index;

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
	private void removeIndexParent(Index index) {
		Index indexParent = index.getIndexOfParent();
		indexParent.setIndexOfChildren(index);
	}
	
	private void removeIndexChildren(ModelConstants modelConstants, Index index) {
		for (Index indexChild: index.getIndexOfChildren()) {
			if (indexChild.getIndexOfChildren().size() != 0) {
				removeIndexChildren(modelConstants, indexChild);
			}
			modelConstants.getListMetadata().remove(indexChild);
		}
	}
	
	@Override
	protected void addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) {

		this.date = contentAnnotation[0];
	}
	
	@Override
	protected boolean removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		this.removeIndexParent(indexOfDate);
		this.removeIndexChildren(modelConstants, indexOfDate);
				
		modelConstants.getListMetadata().remove(indexOfDate);
		
		return true;
	}
	
	@Override
	protected String getValue() {
		
		String chaine = "";
		if (this.indexOfDate != null) {
			chaine += " (nested)";
		}
		chaine += ":\n";
		
		chaine += "\t\t" + "Date : " + this.date + "\n";
		
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
}