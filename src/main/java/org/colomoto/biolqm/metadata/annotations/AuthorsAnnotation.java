package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.Author;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.Index;

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
	
	private Map<Author, Index> listOfIndexOfAuthors;
	
	// constructors
	protected AuthorsAnnotation() {
		this.listOfAuthors = new ArrayList<Author>();
		
		this.listOfIndexOfAuthors = new HashMap<Author, Index>();
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

		Author author = new Author(contentAnnotation[0], contentAnnotation[1], contentAnnotation[2], contentAnnotation[3], contentAnnotation[4]);
		if (!this.listOfAuthors.contains(author)) {
			System.out.println("This value has not been defined yet for this qualifier." + "\n");
		}
		else {
			if (this.listOfIndexOfAuthors.containsKey(author)) {
				Index index = this.listOfIndexOfAuthors.get(author);
				
				this.removeIndexParent(index);
				this.removeIndexChildren(modelConstants, index);
						
				this.listOfIndexOfAuthors.remove(author);
				modelConstants.getListMetadata().remove(index);
			}
			
			this.listOfAuthors.remove(author);
		}
		
		if (this.listOfAuthors.size() == 0) {
			return true;
		}
		return false;
	}

	@Override
	protected String getValue() {
		String chaine = "";
		
		chaine += "\tAuthors :\n";
		for (Author author : this.listOfAuthors) {
			chaine += "\t\t" + author.getName() + ", " + author.getSurname() + ", " + author.getEmail() + ", " + author.getOrganisation() + ", " + author.getOrcid() + "\n";
		}
		
		return chaine;
	}
	
	@Override
	protected Index getIndex(ModelConstants modelConstants, Index indexParent, String[] contentAnnotation) {
		Author author = new Author(contentAnnotation[0], contentAnnotation[1], contentAnnotation[2], contentAnnotation[3], contentAnnotation[4]);
		
		if (!this.listOfAuthors.contains(author)) {
			System.out.println("This author has not been defined yet for this qualifier, so there can be no metadata object attached to it." + "\n");
			return null;
		}
		else {
			Index existingIndex = null;
			
			if (this.listOfIndexOfAuthors.containsKey(author)) {
				existingIndex = this.listOfIndexOfAuthors.get(author);
			}
			else {
				existingIndex = new Index(indexParent, modelConstants.getIncrement());
				
				indexParent.setIndexOfChildren(existingIndex);
						
				this.listOfIndexOfAuthors.put(author, existingIndex);
				modelConstants.getListMetadata().put(existingIndex, new Metadata(modelConstants, "nested"));
			}
			
			return existingIndex;
		}
	}
}