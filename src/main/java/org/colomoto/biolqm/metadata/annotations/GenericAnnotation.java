package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.URI;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.ExternalMetadata;
import org.colomoto.biolqm.metadata.constants.QualifiersAvailable;
import org.colomoto.biolqm.metadata.constants.TagsKeysAvailable;
import org.colomoto.biolqm.metadata.constants.Index;

import java.util.HashSet;
import java.util.HashMap;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Generic class for the annotations
 * This contains pairs of collection-identifier, a list of tags and pairs of key-value
 *
 * @author Martin Boutroux
 */
class GenericAnnotation extends Annotation {
	
	// variables
	private Set<URI> listOfURIs;
	private Set<String> listOfTags;
	private Map<String, ArrayList<String>> listOfKeysValues;
	
	private Index indexOfGeneric;
	
	// constructors
	protected GenericAnnotation() {
		this.listOfURIs = new HashSet<URI>();
		this.listOfTags = new HashSet<String>();
		this.listOfKeysValues = new HashMap<String, ArrayList<String>>();
		
		this.indexOfGeneric = null;
	}
	
	// getters
	protected Set getListOfURIs() {
		return this.listOfURIs;
	}
	
	protected Set<String> getListOfTags() {
		return this.listOfTags;
	}
	
	protected Map<String, ArrayList<String>> getListOfKeysValues() {
		return this.listOfKeysValues;
	}

	// functions
	@Override
	protected void addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) {
		switch (contentAnnotation[0]) {
			case "uri":
				URI uri = new URI(contentAnnotation[1], contentAnnotation[2]);
				this.listOfURIs.add(uri);
				
				modelConstants.getInstanceOfQualifiersAvailable().updateCollections(component, termDesired, contentAnnotation[1]);
				
				if (termDesired == "isDescribedBy") {
					modelConstants.getInstanceOfExternalMetadata().updateExternalMetadata(uri, "bibtex");
				}
				break;
			case "tag":
				String tag = contentAnnotation[1];
				this.listOfTags.add(tag);
				
				modelConstants.getInstanceOfTagsKeysAvailable().updateTagsAvailable(tag);
				break;
			case "keyvalue":
				// if the key doesn't exist, we create it and populate it with the values
				// if it exists, we add to the appropriate key the values which are missing
				
				String key = contentAnnotation[1];
				String value = contentAnnotation[2];
				
				if (this.listOfKeysValues.containsKey(key)) {
					this.listOfKeysValues.get(key).add(value);
				}
				else {
					this.listOfKeysValues.put(key, new ArrayList<String>(Arrays.asList(value)));
				}
				modelConstants.getInstanceOfTagsKeysAvailable().updateKeysValuesAvailable(key, new ArrayList<String>(Arrays.asList(value)));
				break;
			default:
				System.out.println("You have to specify the type of annotation you want to create after the qualifier : uri, tag or keyvalue." + "\n");
				break;
		}
	}
	
	@Override
	protected void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		switch (contentAnnotation[0]) {
			case "uri":
				URI uri = new URI(contentAnnotation[1], contentAnnotation[2]);
				if (!this.listOfURIs.contains(uri)) {
					System.out.println("This uri has not been defined yet for this qualifier." + "\n");
				}
				else {
					this.listOfURIs.remove(uri);
				}
				break;
			case "tag":
				String tag = contentAnnotation[1];
				if (!this.listOfTags.contains(tag)) {
					System.out.println("This tag has not been defined yet for this qualifier." + "\n");
				}
				else {
					this.listOfTags.remove(tag);
				}
				break;
			case "keyvalue":
				String key = contentAnnotation[1];
				String value = contentAnnotation[2];
				if (!this.listOfKeysValues.containsKey(key)) {
					System.out.println("This key has not been defined yet for this qualifier." + "\n");
				}
				else if (!this.listOfKeysValues.get(key).contains(value)) {
					System.out.println("This pair key-value has not been defined yet for this qualifier." + "\n");
				}
				else {					
					this.listOfKeysValues.get(key).remove(value);
					
					if (this.listOfKeysValues.get(key).size() == 0) {
						this.listOfKeysValues.remove(key);
					}
				}
				break;
		}
	}
	
	@Override
	protected String getValue() {
		
		String chaine = "";
		if (this.indexOfGeneric != null) {
			chaine += " (nested)";
		}
		chaine += ":\n";
		
		chaine += "\tURIs :\n";
		for (URI uri : this.listOfURIs) {
			chaine += "\t\t" + uri.getCollection() + ":" + uri.getIdentifier() + "\n";
		}
		
		chaine += "\tTags :\n";
		for (String tag : this.listOfTags) {
			chaine += "\t\t" + tag + "\n";
		}
		
		chaine += "\tKeysValues :\n";
		for (String key : this.listOfKeysValues.keySet()) {
			
			String joint = "";
			
			for (String value: this.listOfKeysValues.get(key)) {
				if (joint != "") { 
					joint += ", "; 
				}
				
				joint += value;
			}
			
			chaine += "\t\t" + key + ":[" + joint + "]\n";
		}
		
		return chaine;
	}
	
	@Override
	protected boolean isSetIndex(ModelConstants modelConstants, Index indexParent) {
		if (this.indexOfGeneric != null) {
			return true;
		}
		return false;
	}
	
	@Override
	protected Index getIndex(ModelConstants modelConstants, Index indexParent) {

		Index existingIndex;
		
		if (this.indexOfGeneric != null) {
			existingIndex = this.indexOfGeneric;
		}
		else {
			existingIndex = new Index(indexParent, modelConstants.getIncrement());
			
			indexParent.setIndexOfChildren(existingIndex);
			
			this.indexOfGeneric = existingIndex;
			modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
		}
		
		return existingIndex;
	}
	
	@Override
	protected ArrayList<ArrayList<String>> getResources() {
		ArrayList<ArrayList<String>> resources = new ArrayList<ArrayList<String>>();
		
		for (URI uri : this.listOfURIs) {
			ArrayList<String> resource = new ArrayList<String>();
			resource.add(uri.getCollection());
			resource.add(uri.getIdentifier());
			
			resources.add(resource);
		}
		
		return resources;
	}
}