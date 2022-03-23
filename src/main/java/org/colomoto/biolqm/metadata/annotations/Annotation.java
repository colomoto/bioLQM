package org.colomoto.biolqm.metadata.annotations;

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
	protected abstract boolean addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) throws Exception;
	protected abstract void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation);
	protected abstract String getValue(String tab);
	protected abstract boolean isSetIndex(ModelConstants modelConstants, Index indexParent);
	protected abstract Index getIndex(ModelConstants modelConstants, Index indexParent) throws Exception;
	protected abstract ArrayList<ArrayList<String>> getResources();
	protected abstract JSONObject getJSONOfAnnotation();
	protected abstract boolean doesAlternativeExist(JSONObject jsonAlternative);
	protected abstract String getShortDescription();
	protected abstract boolean isNotEmpty();
	protected abstract boolean sameAnnotation(Object obj);
}