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
	
	private Map<URI, Index> listOfIndexOfURIs;
	private Map<String, Index> listOfIndexOfTags;
	private Map<Map<String, String>, Index> listOfIndexOfKeysValues;
	
	// constructors
	protected GenericAnnotation() {
		this.listOfURIs = new HashSet<URI>();
		this.listOfTags = new HashSet<String>();
		this.listOfKeysValues = new HashMap<String, ArrayList<String>>();
		
		this.listOfIndexOfURIs = new HashMap<URI, Index>();
		this.listOfIndexOfTags = new HashMap<String, Index>();
		this.listOfIndexOfKeysValues = new HashMap<Map<String, String>, Index>();
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
	protected boolean removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		switch (contentAnnotation[0]) {
			case "uri":
				URI uri = new URI(contentAnnotation[1], contentAnnotation[2]);
				if (!this.listOfURIs.contains(uri)) {
					System.out.println("This uri has not been defined yet for this qualifier." + "\n");
				}
				else {
					if (this.listOfIndexOfURIs.containsKey(uri)) {
						Index index = this.listOfIndexOfURIs.get(uri);
						
						this.removeIndexParent(index);
						this.removeIndexChildren(modelConstants, index);
						
						this.listOfIndexOfURIs.remove(uri);
						modelConstants.getListMetadata().remove(index);
					}
					
					this.listOfURIs.remove(uri);
				}
				break;
			case "tag":
				String tag = contentAnnotation[1];
				if (!this.listOfTags.contains(tag)) {
					System.out.println("This tag has not been defined yet for this qualifier." + "\n");
				}
				else {
					if (this.listOfIndexOfTags.containsKey(tag)) {
						Index index = this.listOfIndexOfTags.get(tag);
						
						this.removeIndexParent(index);
						this.removeIndexChildren(modelConstants, index);
						
						this.listOfIndexOfTags.remove(tag);
						modelConstants.getListMetadata().remove(index);
					}
					
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
					Map<String, String> obj = new HashMap<String, String>();
					obj.put(key, value);
					
					if (this.listOfIndexOfKeysValues.containsKey(obj)) {
						Index index = this.listOfIndexOfKeysValues.get(obj);
						
						this.removeIndexParent(index);
						this.removeIndexChildren(modelConstants, index);
						
						this.listOfIndexOfKeysValues.remove(obj);
						modelConstants.getListMetadata().remove(index);
					}
					
					this.listOfKeysValues.get(key).remove(value);
					
					if (this.listOfKeysValues.get(key).size() == 0) {
						this.listOfKeysValues.remove(key);
					}
				}
				break;
		}
		
		if (this.listOfURIs.size() == 0 && this.listOfTags.size() == 0 && this.listOfKeysValues.size() == 0) {
			return true;
		}
		return false;
	}
	
	@Override
	protected String getValue() {
		String chaine = "";
		
		chaine += "\tURIs :\n";
		for (URI uri : this.listOfURIs) {
			String nested = "";
			if (this.listOfIndexOfURIs.containsKey(uri)) {
				nested = " (nested)";
			}
			
			chaine += "\t\t" + uri.getCollection() + ":" + uri.getIdentifier() + nested + "\n";
		}
		
		chaine += "\tTags :\n";
		for (String tag : this.listOfTags) {
			String nested = "";
			if (this.listOfIndexOfTags.containsKey(tag)) {
				nested = " (nested)";
			}
			
			chaine += "\t\t" + tag + nested + "\n";
		}
		
		chaine += "\tKeysValues :\n";
		for (String key : this.listOfKeysValues.keySet()) {
			
			String joint = "";
			
			for (String value: this.listOfKeysValues.get(key)) {
				if (joint != "") { 
					joint += ", "; 
				}
				
				joint += value;
				
				Map<String, String> obj = new HashMap<String, String>();
				obj.put(key, value);
				if (this.listOfIndexOfKeysValues.containsKey(obj)) {
					joint += " (nested)";
				}
			}
			
			chaine += "\t\t" + key + ":[" + joint + "]\n";
		}
		
		return chaine;
	}
	
	@Override
	protected Index getIndex(ModelConstants modelConstants, Index indexParent, String[] contentAnnotation) {
		switch (contentAnnotation[0]) {
			case "uri":
				URI uri = new URI(contentAnnotation[1], contentAnnotation[2]);
				
				if (!this.listOfURIs.contains(uri)) {
					System.out.println("This uri has not been defined yet for this qualifier, so there can be no metadata object attached to it." + "\n");
					return null;
				}
				else {
					Index existingIndex = null;
					
					if (this.listOfIndexOfURIs.containsKey(uri)) {
						existingIndex = this.listOfIndexOfURIs.get(uri);
					}
					else {
						existingIndex = new Index(indexParent, modelConstants.getIncrement());
						
						indexParent.setIndexOfChildren(existingIndex);
						
						this.listOfIndexOfURIs.put(uri, existingIndex);
						modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
					}
					
					return existingIndex;
				}
			case "tag":
				String tag = contentAnnotation[1];
				if (!this.listOfTags.contains(tag)) {
					System.out.println("This tag has not been defined yet for this qualifier, so there can be no metadata object attached to it." + "\n");
					return null;
				}
				else {
					Index existingIndex = null;
					
					if (this.listOfIndexOfTags.containsKey(tag)) {
						existingIndex = this.listOfIndexOfTags.get(tag);
					}
					else {
						existingIndex = new Index(indexParent, modelConstants.getIncrement());
						
						indexParent.setIndexOfChildren(existingIndex);
						
						this.listOfIndexOfTags.put(tag, existingIndex);
						modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
					}
					
					return existingIndex;
				}
			case "keyvalue":
				String key = contentAnnotation[1];
				String value = contentAnnotation[2];
				if (!this.listOfKeysValues.containsKey(key)) {
					System.out.println("This key has not been defined yet for this qualifier, so there can be no metadata object attached to it." + "\n");
					return null;
				}
				else if (!this.listOfKeysValues.get(key).contains(value)) {
					System.out.println("This pair key-value has not been defined yet for this qualifier." + "\n");
				}
				else {
					Index existingIndex = null;
					
					Map<String, String> obj = new HashMap<String, String>();
					obj.put(key, value);
					
					if (this.listOfIndexOfKeysValues.containsKey(obj)) {
						existingIndex = this.listOfIndexOfKeysValues.get(obj);
					}
					else {
						existingIndex = new Index(indexParent, modelConstants.getIncrement());
						
						indexParent.setIndexOfChildren(existingIndex);
						
						this.listOfIndexOfKeysValues.put(obj, existingIndex);
						modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
					}
					
					return existingIndex;
				}
		}
		
		return null;
	}
}