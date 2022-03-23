package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.Index;

import org.json.JSONObject;
import org.json.JSONArray;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/**
 * Special class of annotations
 * This contains the annotations relative to the authors of the model
 *
 * @author Martin Boutroux
 */
class AuthorsAnnotation extends Annotation {
	
	// variables
	private Set<Author> listOfAuthors;
	
	private Index indexOfAuthors;
	
	// constructors
	protected AuthorsAnnotation() {
		this.listOfAuthors = new HashSet<Author>();
		
		this.indexOfAuthors = null;
	}
		
	// getters
	protected Set<Author> getListOfAuthors() {
		return this.listOfAuthors;
	}
	
	// functions
	@Override
	protected boolean addAnnotation(ModelConstants modelConstants, String component, String termDesired, String[] contentAnnotation) throws Exception {

		Author author = new Author(contentAnnotation[0], contentAnnotation[1], contentAnnotation[2], contentAnnotation[3], contentAnnotation[4]);
		if (this.listOfAuthors.contains(author)) {
			throw new Exception("The author could not be added because it already exists.");
		}
		this.listOfAuthors.add(author);
		
		return true;
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
	protected String getValue(String tab) {
		
		String chaine = ":\n";
		
		chaine += tab + "\tAuthors :\n";
		for (Author author : this.listOfAuthors) {
			chaine += tab + "\t\t" + author.getName() + ", " + author.getSurname() + ", " + author.getEmail() + ", " + author.getOrganisation() + ", " + author.getOrcid() + "\n";
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
	protected Index getIndex(ModelConstants modelConstants, Index indexParent) throws Exception {

		Index existingIndex;
		
		if (this.indexOfAuthors != null) {
			existingIndex = this.indexOfAuthors;
		}
		else {
			existingIndex = new Index(indexParent, modelConstants.getIncrement());
			
			indexParent.setIndexOfChildren(existingIndex);
			
			this.indexOfAuthors = existingIndex;
			
			// we get the type of the parent Metadata
			String type = modelConstants.getListMetadata().get(indexParent).getType();
			
			modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, type, true));
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
				jsonAuthor.put("email", author.getEmail());
				jsonAuthor.put("organisation", author.getOrganisation());
				jsonAuthor.put("orcid", author.getOrcid());
				
				arrayAuthors.put(jsonAuthor);
			}
			
			json.put("authors", arrayAuthors);
		}
		
		return json;
	}
	
	@Override
	protected boolean doesAlternativeExist(JSONObject jsonAlternative) {
		
		JSONArray arrayAuthors = jsonAlternative.getJSONArray("authors");
		for(int idAuthor = 0; idAuthor < arrayAuthors.length(); idAuthor++)
		{
			JSONObject author = arrayAuthors.getJSONObject(idAuthor);
			
			String email = null;
			if (author.has("email") && !author.isNull("email")) { email = author.getString("email"); }
			String organisation = null;
			if (author.has("organisation") && !author.isNull("organisation")) { organisation = author.getString("organisation"); }
			String orcid = null;
			if (author.has("orcid") && !author.isNull("orcid")) { orcid = author.getString("orcid"); }
			
			Author authorObject = new Author(author.getString("name"), author.getString("surname"), email, organisation, orcid);
			if (!this.listOfAuthors.contains(authorObject)) {
				return false;
			}
		}
		
		return true;
	}
	
	@Override
	protected String getShortDescription() {
		if (this.listOfAuthors.size() == 1) {
			return this.listOfAuthors.size()+" author";
		}
		return this.listOfAuthors.size()+" authors";
	}
	
	@Override
	public boolean isNotEmpty() {
		if (this.listOfAuthors.size()>0) {
			return true;
		}
		return false;
	}
	
	@Override
	public boolean sameAnnotation(Object obj) {
		
		AuthorsAnnotation auth = (AuthorsAnnotation) obj;
		
		if (this.listOfAuthors.size() != auth.listOfAuthors.size()) {
			System.err.println("The two annotations does not contain the same number of authors.");
			return false;
		}
		
		for (Author author: this.listOfAuthors) {
			if (!auth.getListOfAuthors().contains(author)) {
				System.err.println("An author does not match.");
				return false;
			}
		}

		return true;
	}
}