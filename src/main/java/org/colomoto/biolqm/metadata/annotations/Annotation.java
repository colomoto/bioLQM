package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.Index;

import org.json.JSONObject;

import java.util.ArrayList;

/**
 * Abstract class for the annotations
 * All the types of annotations extend from this class
 *
 * @author Martin Boutroux
 */
abstract class Annotation {
	
	// functions
	protected abstract void addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation);
	protected abstract void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation);
	protected abstract String getValue(String tab);
	protected abstract boolean isSetIndex(ModelConstants modelConstants, Index indexParent);
	protected abstract Index getIndex(ModelConstants modelConstants, Index indexParent);
	protected abstract ArrayList<ArrayList<String>> getResources();
	protected abstract JSONObject getJSONOfAnnotation();
}
