package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.AnnotationModule;
import org.colomoto.biolqm.metadata.constants.Author;
import org.json.JSONArray;
import org.json.JSONObject;

import java.time.LocalDate;
import java.util.*;

/**
 * Special class of annotations for authorship and distribution terms
 *
 * @author Martin Boutroux
 */
public class LegalAnnotation {

	private final Set<Author> authors;
	private LocalDate creationDate, modificationDate;
	private String distribution;

	public LegalAnnotation() {
		this.authors = new HashSet<>();
	}

	protected Set<Author> getAuthors() {
		return this.authors;
	}
	
	protected boolean addAuthor(AnnotationModule annotModule, String[] contentAnnotation) throws Exception {

		Author author = new Author(contentAnnotation[0], contentAnnotation[1], contentAnnotation[2], contentAnnotation[3], contentAnnotation[4]);
		if (this.authors.contains(author)) {
			return false;
		}
		this.authors.add(author);
		
		return true;
	}

	protected void removeAnnotation(String[] contentAnnotation) {

		Author author = new Author(contentAnnotation[0], contentAnnotation[1], contentAnnotation[2], contentAnnotation[3], contentAnnotation[4]);
		if (!this.authors.contains(author)) {
			System.err.println("This value has not been defined yet for this qualifier." + "\n");
		}
		else {
			this.authors.remove(author);
		}
	}

	protected String getValue(String tab) {
		
		StringBuilder chaine = new StringBuilder(":\n" + tab + "\tAuthors :\n");
		for (Author author : this.authors) {
			chaine.append(tab).append("\t\t").append(author.toString()).append("\n");
		}
		return chaine.toString();
	}
	
	protected ArrayList<ArrayList<String>> getResources() {
		ArrayList<ArrayList<String>> resources = new ArrayList<>();
		
		for (Author author : this.authors) {
			ArrayList<String> resource = new ArrayList<>();
			resource.add(author.name);
			resource.add(author.surname);
			resource.add(author.organisation);
			resource.add(author.email);
			
			resources.add(resource);
		}
		
		return resources;
	}
	
	protected JSONObject getJSONOfAnnotation() {
		JSONObject json = new JSONObject();
		
		if (this.authors.size()>0) {
			JSONArray arrayAuthors = new JSONArray();
			
			for (Author author : this.authors) {
				JSONObject jsonAuthor = new JSONObject();
				
				jsonAuthor.put("name", author.name);
				jsonAuthor.put("surname", author.surname);
				jsonAuthor.put("email", author.email);
				jsonAuthor.put("organisation", author.organisation);
				jsonAuthor.put("orcid", author.orcid);
				
				arrayAuthors.put(jsonAuthor);
			}
			
			json.put("authors", arrayAuthors);
		}
		
		return json;
	}
	
	protected String getShortDescription() {
		StringBuilder sb = new StringBuilder();
		switch (this.authors.size()) {
			case 0:
				sb.append("NO author");
				break;
			case 1:
				sb.append("1 author");
				break;
			default:
				sb.append(this.authors.size()).append(" authors");
		}
		if (this.creationDate != null) {
			sb.append(", created");
		}
		if (this.modificationDate != null) {
			sb.append(", modified");
		}
		if (this.distribution != null) {
			sb.append(", distribution");
		}

		return sb.toString();
	}
	
	public boolean isEmpty() {
		return this.authors.isEmpty() && this.creationDate == null && this.modificationDate == null && this.distribution == null;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof LegalAnnotation)) {
			return false;
		}

		LegalAnnotation auth = (LegalAnnotation) obj;
		if (this.authors.size() != auth.authors.size()) {
			return false;
		}
		
		for (Author author: this.authors) {
			if (!auth.getAuthors().contains(author)) {
				return false;
			}
		}

		return Objects.equals(this.creationDate, auth.creationDate)
				&& Objects.equals(this.modificationDate, auth.modificationDate)
				&& Objects.equals(this.distribution, auth.distribution);
	}
}