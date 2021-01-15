package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.Index;

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
	protected boolean removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {

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
}