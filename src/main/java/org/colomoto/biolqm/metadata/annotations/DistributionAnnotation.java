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

		this.distribution = contentAnnotation[0];
	}
	
	@Override
	protected boolean removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		this.removeIndexParent(indexOfDistribution);
		this.removeIndexChildren(modelConstants, indexOfDistribution);
				
		modelConstants.getListMetadata().remove(indexOfDistribution);
		
		return true;
	}
	
	@Override
	protected String getValue() {
		
		String chaine = "";
		if (this.indexOfDistribution != null) {
			chaine += " (nested)";
		}
		chaine += ":\n";
		
		chaine += "\t\t" + "Terms of distribution : " + this.distribution + "\n";
		
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
}