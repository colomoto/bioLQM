package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.URI;
import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.metadata.annotations.JsonReader;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.ExternalMetadata;
import org.colomoto.biolqm.metadata.constants.QualifiersAvailable;
import org.colomoto.biolqm.metadata.constants.TagsKeysAvailable;
import org.colomoto.biolqm.metadata.constants.Index;

import org.json.JSONObject;
import org.json.JSONArray;

import java.util.HashSet;
import java.util.HashMap;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.IOException;

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
	protected Set<URI> getListOfURIs() {
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
					if (contentAnnotation[1] == "doi" && !modelConstants.getInstanceOfExternalMetadata().isSetExternalMetadata(uri)) {
						String url = "https://api.crossref.org/works/"+contentAnnotation[2];
						
						System.out.println("dessous");

						try {
							JSONObject json = JsonReader.readJsonFromUrl(url);
							JSONObject jsonMessage = json.getJSONObject("message");
							
							String title = null;
							Integer year = null;
							if (jsonMessage.has("title") && !jsonMessage.isNull("title")) {
								title = jsonMessage.getJSONArray("title").getString(0).toString();
							}
							if (jsonMessage.has("created") && !jsonMessage.isNull("created")) {
								year = jsonMessage.getJSONObject("created").getJSONArray("date-parts").getJSONArray(0).getInt(0);
							}
							
							if (title != null && year != null) {
								modelConstants.getInstanceOfExternalMetadata().updateExternalMetadata(uri, title, String.valueOf(year));
							}
							else {
								System.err.println("Error retrieving the metadata of the doi: at least one of the characteristics couldn't be fetched." + "\n");
							}	
						} catch (IOException e) {
							System.err.println("Error retrieving the metadata of the doi." + "\n");
						}
						
						System.out.println("dessus");
					}
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
				System.err.println("You have to specify the type of annotation you want to create after the qualifier : uri, tag or keyvalue." + "\n");
				break;
		}
	}
	
	@Override
	protected void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {
		switch (contentAnnotation[0]) {
			case "uri":
				URI uri = new URI(contentAnnotation[1], contentAnnotation[2]);
				if (!this.listOfURIs.contains(uri)) {
					System.err.println("This uri has not been defined yet for this qualifier." + "\n");
				}
				else {
					this.listOfURIs.remove(uri);
				}
				break;
			case "tag":
				String tag = contentAnnotation[1];
				if (!this.listOfTags.contains(tag)) {
					System.err.println("This tag has not been defined yet for this qualifier." + "\n");
				}
				else {
					this.listOfTags.remove(tag);
				}
				break;
			case "keyvalue":
				String key = contentAnnotation[1];
				String value = contentAnnotation[2];
				if (!this.listOfKeysValues.containsKey(key)) {
					System.err.println("This key has not been defined yet for this qualifier." + "\n");
				}
				else if (!this.listOfKeysValues.get(key).contains(value)) {
					System.err.println("This pair key-value has not been defined yet for this qualifier." + "\n");
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
	protected String getValue(String tab) {
		
		String chaine = ":\n";
		
		chaine += tab + "\tURIs :\n";
		for (URI uri : this.listOfURIs) {
			chaine += tab + "\t\t" + uri.getCollection() + ":" + uri.getIdentifier() + "\n";
		}
		
		chaine += tab + "\tTags :\n";
		for (String tag : this.listOfTags) {
			chaine += tab + "\t\t" + tag + "\n";
		}
		
		chaine += tab + "\tKeysValues :\n";
		for (String key : this.listOfKeysValues.keySet()) {
			
			String joint = "";
			
			for (String value: this.listOfKeysValues.get(key)) {
				if (joint != "") { 
					joint += ", "; 
				}
				
				joint += value;
			}
			
			chaine += tab + "\t\t" + key + ":[" + joint + "]\n";
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
	
	@Override
	protected JSONObject getJSONOfAnnotation() {
		JSONObject json = new JSONObject();
		
		if (this.listOfURIs.size()>0) {
			JSONArray arrayURIs = new JSONArray();
			
			for (URI uri : this.listOfURIs) {
				JSONObject jsonURI = new JSONObject();
				
				jsonURI.put("collection", uri.getCollection());
				jsonURI.put("identifier", uri.getIdentifier());
				
				arrayURIs.put(jsonURI);
			}
			
			json.put("uris", arrayURIs);
		}
		
		if (this.listOfTags.size()>0) {
			JSONArray arrayTags = new JSONArray();
			
			for (String tag : this.listOfTags) {
				arrayTags.put(tag);
			}
			
			json.put("tags", arrayTags);
		}
		
		if (this.listOfKeysValues.size()>0) {
			JSONArray arrayKeysValues = new JSONArray();
			
			for (String key : this.listOfKeysValues.keySet()) {
				
				JSONObject jsonKey = new JSONObject();
				
				jsonKey.put("key", key);
				jsonKey.put("values", this.listOfKeysValues.get(key));
				
				arrayKeysValues.put(jsonKey);
			}
			
			json.put("keysvalues", arrayKeysValues);
		}
		
		return json;
	}
		
	@Override
	protected boolean doesAlternativeExist(JSONObject jsonAlternative) {
		
		int numUriAnnotation = this.listOfURIs.size();
		if (jsonAlternative.has("uris") && !jsonAlternative.isNull("uris")) {
			JSONArray arrayURIs = jsonAlternative.getJSONArray("uris");
			
			int numUriJson = arrayURIs.length();
			if (numUriAnnotation == numUriJson) {
				
				for(int idUri = 0; idUri < arrayURIs.length(); idUri++)
				{
					JSONObject jsonURI = arrayURIs.getJSONObject(idUri);
					URI uri = new URI(jsonURI.getString("collection"), jsonURI.getString("identifier"));
					if (!this.listOfURIs.contains(uri)) {
						return false;
					}
				}
			}
			else {
				return false;
			}
		}
		else if (numUriAnnotation != 0) {
			return false;
		}
		
		int numTagAnnotation = this.listOfTags.size();
		if (jsonAlternative.has("tags") && !jsonAlternative.isNull("tags")) {
			JSONArray arrayTags = jsonAlternative.getJSONArray("tags");
			
			int numTagJson = arrayTags.length();
			if (numTagAnnotation == numTagJson) {
				
				for(int idTag = 0; idTag < arrayTags.length(); idTag++)
				{
					String tag = arrayTags.getString(idTag);
					if (!this.listOfTags.contains(tag)) {
						return false;
					}
				}
			}
			else {
				return false;
			}
		}
		else if (numTagAnnotation != 0) {
			return false;
		}
		
		int numKeyAnnotation = this.listOfKeysValues.size();
		if (jsonAlternative.has("keysvalues") && !jsonAlternative.isNull("keysvalues")) {
			JSONArray arrayKeys = jsonAlternative.getJSONArray("keysvalues");
			
			int numKeyJson = arrayKeys.length();
			if (numKeyAnnotation == numKeyJson) {
				
				for(int idKey = 0; idKey < arrayKeys.length(); idKey++)
				{
					JSONObject key = arrayKeys.getJSONObject(idKey);
					JSONArray arrayValues = key.getJSONArray("values");
					
					String keyString = key.getString("key");
					if (!this.listOfKeysValues.containsKey(keyString)) {
						return false;
					}
					
					ArrayList<String> valuesList = this.listOfKeysValues.get(keyString);
					
					int numValuesAnnotation = valuesList.size();
					int numValuesJson = arrayValues.length();
					if (numValuesAnnotation == numValuesJson) {
						
						for (int idValue = 0; idValue < arrayValues.length(); idValue++) {
							String valueString =  arrayValues.getString(idValue);
							if (!valuesList.contains(valueString)) {
								return false;
							}
						}
					}
					else {
						return false;
					}
				}
			}
			else {
				return false;
			}
		}
		else if (numKeyAnnotation != 0) {
			return false;
		}
		
		return true;
	}
}