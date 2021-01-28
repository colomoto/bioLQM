package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.Author;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.Index;

import org.json.JSONObject;
import org.json.JSONArray;

import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;

/**
 * Special class of annotations
 * This contains the annotations relative to the authors of the model
 *
 * @author Martin Boutroux
 */
class AuthorsAnnotation extends Annotation {
	
	// variables
	private ArrayList<Author> listOfAuthors;
	
	private Index indexOfAuthors;
	
	// constructors
	protected AuthorsAnnotation() {
		this.listOfAuthors = new ArrayList<Author>();
		
		this.indexOfAuthors = null;
	}
		
	// getters
	protected ArrayList<Author> getListOfAuthors() {
		return this.listOfAuthors;
	}
	
	// functions
	@Override
	protected void addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) {

		Author author = new Author(contentAnnotation[0], contentAnnotation[1], contentAnnotation[2], contentAnnotation[3], contentAnnotation[4]);
		this.listOfAuthors.add(author);
	}

	@Override
	protected void removeAnnotation(ModelConstants modelConstants, String[] contentAnnotation) {

		Author author = new Author(contentAnnotation[0], contentAnnotation[1], contentAnnotation[2], contentAnnotation[3], contentAnnotation[4]);
		if (!this.listOfAuthors.contains(author)) {
			System.err.println("This value has not been defined yet for this qualifier." + "\n");
		}
		else {		
			this.listOfAuthors.remove(author);
		}
	}

	@Override
	protected String getValue() {
		
		String chaine = "";
		if (this.indexOfAuthors != null) {
			chaine += " (nested)";
		}
		chaine += ":\n";
		
		chaine += "\tAuthors :\n";
		for (Author author : this.listOfAuthors) {
			chaine += "\t\t" + author.getName() + ", " + author.getSurname() + ", " + author.getEmail() + ", " + author.getOrganisation() + ", " + author.getOrcid() + "\n";
		}
		
		return chaine;
	}
	
	@Override
	protected boolean isSetIndex(ModelConstants modelConstants, Index indexParent) {
		if (this.indexOfAuthors != null) {
			return true;
		}
		return false;
	}
	
	@Override
	protected Index getIndex(ModelConstants modelConstants, Index indexParent) {

		Index existingIndex;
		
		if (this.indexOfAuthors != null) {
			existingIndex = this.indexOfAuthors;
		}
		else {
			existingIndex = new Index(indexParent, modelConstants.getIncrement());
			
			indexParent.setIndexOfChildren(existingIndex);
			
			this.indexOfAuthors = existingIndex;
			modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
		}
		
		return existingIndex;
	}
	
	@Override
	protected ArrayList<ArrayList<String>> getResources() {
		ArrayList<ArrayList<String>> resources = new ArrayList<ArrayList<String>>();
		
		for (Author author : this.listOfAuthors) {
			ArrayList<String> resource = new ArrayList<String>();
			resource.add(author.getName());
			resource.add(author.getSurname());
			resource.add(author.getOrganisation());
			resource.add(author.getEmail());
			
			resources.add(resource);
		}
		
		return resources;
	}
	
	@Override
	protected JSONObject getJSONOfAnnotation() {
		JSONObject json = new JSONObject();
		
		if (this.listOfAuthors.size()>0) {
			JSONArray arrayAuthors = new JSONArray();
			
			for (Author author : this.listOfAuthors) {
				JSONObject jsonAuthor = new JSONObject();
				
				jsonAuthor.put("name", author.getName());
				jsonAuthor.put("surname", author.getSurname());
				jsonAuthor.put("mail", author.getEmail());
				jsonAuthor.put("organisation", author.getOrganisation());
				jsonAuthor.put("orcid", author.getOrcid());
				
				arrayAuthors.put(jsonAuthor);
			}
			
			json.put("authors", arrayAuthors);
		}
		
		return json;
	}
	
	@Override
	protected boolean isAnnotationNotEmpty() {
		if (this.listOfAuthors.size()>0) {
			return true;
		}
		return false;
	}
}