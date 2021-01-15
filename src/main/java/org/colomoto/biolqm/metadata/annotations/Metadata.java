package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.AnnotationFactory;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.QualifiersAvailable;
import org.colomoto.biolqm.metadata.constants.TagsKeysAvailable;
import org.colomoto.biolqm.metadata.constants.Index;
import org.colomoto.biolqm.metadata.constants.Qualifier;

import org.colomoto.biolqm.metadata.validations.DateValidator;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.time.LocalDate;

/**
 * One instance per component (model, node, transition...)
 * It contains the annotations and the notes relative to a component
 *
 * @author Martin Boutroux
 */
public class Metadata {
	
	// variables
	private String type;
	private String notes;
	private Map<String, Annotation> listOfAnnotations;
	
	private ModelConstants modelConstants;
	
	
	// constructors
	public Metadata(ModelConstants newModelConstants, String newType)
	{
		this.type = newType;
		this.notes = "";
		this.listOfAnnotations = new HashMap<String, Annotation>();
		
		this.modelConstants = newModelConstants;
		
		if (newType == "model") {
			LocalDate currentDate = LocalDate.now();
			this.addDate("created", currentDate.toString());
			this.addDate("modified", currentDate.toString());
		}
	}
	
	
	// getters
	/**
	 * Retrieve the component's notes
	 */	
	public String getNotes() {
		return this.notes;
	}
	
	
	// setters
	/**
	 * Update the component's notes
	 */
	public void setNotes(String newNotes) {
		this.notes = newNotes;
	}
	
	
	// functions
	private String suitedJavaClass(String termDesired) {
		Map<String, Qualifier> listQualifiersComponent = this.modelConstants.getQualifiersAvailable(this.type);
		
		if (listQualifiersComponent.containsKey(termDesired)) {
			Qualifier qualifierDesired = listQualifiersComponent.get(termDesired);
			
			if (qualifierDesired.getJavaClass() != null) {
				return qualifierDesired.getJavaClass();
			}
			else {
				return "GenericAnnotation";
			}
		}
		
		return null;
	}
	
	private Index getLocalIndex() {
		for (Entry<Index, Metadata> entry : this.modelConstants.getListMetadata().entrySet()) {
			if (entry.getValue().equals(this)) {
				return entry.getKey();
			}
		}
		return null;
	}
	
	private boolean isValidEmail(String email) {
		if (email != null) {
			return email.matches("^(.+)@(.+)$");
		}
		return true;
	}
	
	private boolean isValidOrcid(String orcid) {
		if (orcid != null) {
			return orcid.matches("\\d{4}-\\d{4}-\\d{4}-\\d{4}");
		}
		return true;
	}


	// the functions to add an annotation
	private void addAnnotation(String termDesired, String javaClassDesired, String... contentAnnotation) {

		// we find the javaClass dedicated for this qualifier
		String javaClass = this.suitedJavaClass(termDesired);
		
		// if the qualifier doesn't exist yet we add it to the list of qualifiers available
		if (javaClass == null) {
			this.modelConstants.getInstanceOfQualifiersAvailable().updateQualifiers(this.type, termDesired, "customQualifier", javaClassDesired);
			
			javaClass = javaClassDesired;
		}
		
		// if the javaClass for this qualifier match the javaClass asked by the user we create the annotation
		if (javaClass.equals(javaClassDesired)) {
			if (!this.listOfAnnotations.containsKey(termDesired)) {
				AnnotationFactory factory = new AnnotationFactory();
				Annotation newAnnotation = factory.getInstance(javaClass);
				
				newAnnotation.addAnnotation(this.modelConstants, this.type, termDesired, contentAnnotation);
				
				this.listOfAnnotations.put(termDesired, newAnnotation);
			}
			else {
				this.listOfAnnotations.get(termDesired).addAnnotation(this.modelConstants, this.type, termDesired, contentAnnotation);
			}
		}
		// else we print a warning
		else {
			System.out.println("You cannot create this type of annotation for this qualifier." + "\n");
		}
	}
	
	/**
	 * Add a new URI to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param collection the collection of the uri one wants to create (uniprot, chebi...)
	 * @param identifier the entry one wants to point at in the collection
	 */	
	public void addURI(String termDesired, String collection, String identifier) {
		String javaClassDesired = "GenericAnnotation";
		this.addAnnotation(termDesired, javaClassDesired, "uri", collection, identifier);
	}
	/**
	 * Add a new tag to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param tag the tag one wants to add
	 */	
	public void addTag(String termDesired, String tag) {
		String javaClassDesired = "GenericAnnotation";
		this.addAnnotation(termDesired, javaClassDesired, "tag", tag);
	}
	/**
	 * Add a new pair key-value to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param key the place where one wants to store the value
	 * @param value the value one wants to store
	 */	
	public void addKeyValue(String termDesired, String key, String value) {
		String javaClassDesired = "GenericAnnotation";
		this.addAnnotation(termDesired, javaClassDesired, "keyvalue", key, value);
	}
	/**
	 * Add a new author to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param name the name of the author
	 * @param surname the surname of the author
	 * @param email the email of the author
	 * @param organisation the organisation of the author
	 * @param orcid the orcid of the author
	 */	
	public void addAuthor(String termDesired, String name, String surname, String email, String organisation, String orcid) {
		if (!this.isValidEmail(email)) {
			System.out.println("The email is not valid. It should contain an @ (at the very least)" + "\n");
			return;
		}
		if (!this.isValidOrcid(orcid)) {
			System.out.println("The orcid is not valid. It should follow the format ****-****-****-**** with * a number" + "\n");
			return;
		}
		
		String javaClassDesired = "AuthorsAnnotation";
		this.addAnnotation(termDesired, javaClassDesired, name, surname, email, organisation, orcid);
	}
	/**
	 * Add a new date to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param date the date one wants to add
	 */	
	public void addDate(String termDesired, String date) {
		DateValidator validator = new DateValidator("yyyy-MM-dd");

		if (validator.isValid(date)) { 		
			String javaClassDesired = "DateAnnotation";
			this.addAnnotation(termDesired, javaClassDesired, date);
		}
		else {
			System.out.println("The date is not valid. It should follow the format YYYY-MM-DD" + "\n");
		}
	}
	/**
	 * Add new terms of distribution to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param distribution the terms of distribution one wants to add
	 */	
	public void addDistribution(String termDesired, String distribution) {
		String javaClassDesired = "DistributionAnnotation";
		this.addAnnotation(termDesired, javaClassDesired, distribution);
	}
	
	
	// the functions to remove an annotation
	private void removeAnnotation(String termDesired, String javaClassDesired, String... contentAnnotation) {
		// we find the javaClass dedicated for this qualifier
		String javaClass = this.suitedJavaClass(termDesired);
		
		if (javaClass == null) {
			System.out.println("This qualifier doesn't exist." + "\n");
		}
		else if (javaClass.equals(javaClassDesired)) {
			if (!this.listOfAnnotations.containsKey(termDesired)) {
				System.out.println("This qualifier has not been defined yet." + "\n");
			}
			else {			
				boolean emptyAnnotation = this.listOfAnnotations.get(termDesired).removeAnnotation(this.modelConstants, contentAnnotation);
				
				if (emptyAnnotation) {
					this.listOfAnnotations.remove(termDesired);
				}
			}
		}
		else {
			System.out.println("You cannot remove this kind of annotation for this qualifier." + "\n");
		}
	}
	
	/**
	 * Remove an URI from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param collection the collection of the uri one wants to remove
	 * @param identifier the entry in the collection one wants to remove
	 */	
	public void removeURI(String termDesired, String collection, String identifier) {
		String javaClassDesired = "GenericAnnotation";
		this.removeAnnotation(termDesired, javaClassDesired, "uri", collection, identifier);
	}
	/**
	 * Remove a tag from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param tag the tag one wants to remove
	 */	
	public void removeTag(String termDesired, String tag) {
		String javaClassDesired = "GenericAnnotation";
		this.removeAnnotation(termDesired, javaClassDesired, "tag", tag);
	}
	/**
	 * Remove a pair key-value from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param key the place where one wants to remove a value
	 * @param value the value one wants to remove
	 */	
	public void removeKeyValue(String termDesired, String key, String value) {
		String javaClassDesired = "GenericAnnotation";
		this.removeAnnotation(termDesired, javaClassDesired, "keyvalue", key, value);
	}
	/**
	 * Remove an author from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param name the name of the author
	 * @param surname the surname of the author
	 * @param email the email of the author
	 * @param organisation the organisation of the author
	 * @param orcid the orcid of the author
	 */	
	public void removeAuthor(String termDesired, String name, String surname, String email, String organisation, String orcid) {
		String javaClassDesired = "AuthorsAnnotation";
		this.removeAnnotation(termDesired, javaClassDesired, name, surname, email, organisation, orcid);
	}
	/**
	 * Remove a date from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 */	
	public void removeDate(String termDesired) {
		String javaClassDesired = "DateAnnotation";
		this.removeAnnotation(termDesired, javaClassDesired, "");
	}
	/**
	 * Remove the terms of distribution from the component
	 * 
	 * @param termDesired the qualifier one wants to remove
	 */	
	public void removeDistribution(String termDesired) {
		String javaClassDesired = "DistributionAnnotation";
		this.removeAnnotation(termDesired, javaClassDesired, "");
	}
	
	
	// the functions to get a description of the components' annotations
	/**
	 * Retrieve a String containing the description of an annotation
	 * 
	 * @param termDesired the qualifier one wants to get the description of
	 */	
	public String getDescriptionAnnotation(String termDesired) {
		if (this.listOfAnnotations.containsKey(termDesired)) {
			return termDesired + this.listOfAnnotations.get(termDesired).getValue();
		}
		return "This type of annotation has not been defined for this component" + "\n";
	}

	/**
	 * Retrieve a String containing the description of all the component's annotations
	 * 
	 * @param termDesired the qualifier one wants to get the description of
	 */	
	public String getDescriptionMetadata() {
		String description = "";
		
		Set keys = this.listOfAnnotations.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()) {
			String termDesired = (String) it.next();

			description += termDesired + this.listOfAnnotations.get(termDesired).getValue();
		}
		
		return description;
	}
	
	
	// the functions to get help on how to annotate properly
	/**
	 * Retrieve a String containing suggestions on how to annotate the component for a given qualifier
	 * 
	 * @param termDesired the qualifier one wants to get some help with
	 */	
	public String getHelpAnnotation(String termDesired) {
		String help = "";
		
		if (this.modelConstants.getQualifiersAvailable(this.type).containsKey(termDesired)) {
			help += termDesired + ":\n";
			
			help += this.modelConstants.getInstanceOfQualifiersAvailable().getParametersQualifier(this.type, termDesired);
			
			help += "Characteristics:\n" + this.modelConstants.getInstanceOfQualifiersAvailable().getHelpQualifier(this.type, termDesired);
		
			help += "Tags available:\n" + "\t" + this.modelConstants.getTagsAvailable().toString() + "\n";
			
			help += "Keys-Values available:\n";
			Set keys2 = this.modelConstants.getKeysValuesAvailable().keySet();
			Iterator it2 = keys2.iterator();
			while (it2.hasNext()) {
				String customKey = (String) it2.next();
				
				help += "\t" + customKey + ": " + this.modelConstants.getKeysValuesAvailable().get(customKey) + "\n";
			}
		}
		
		if (help != "") {
			return help;
		}
		
		return "This type of qualifier has not been defined for this component" + "\n";
	}
	
	/**
	 * Retrieve a String containing suggestions on how to annotate the component
	 */	
	public String getHelpMetadata() {		
		String help = "Qualifiers available:\n";
		
		Map<String, Qualifier> listQualifiersComponent = this.modelConstants.getQualifiersAvailable(this.type);
		
		Set keys = listQualifiersComponent.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()) {
			String term = (String) it.next();
			Qualifier qual = listQualifiersComponent.get(term);
			
			help += "-" + term + ":\n" + this.modelConstants.getInstanceOfQualifiersAvailable().getHelpQualifier(this.type, term);
		}
		
		help += "Tags available:\n" + "\t" + this.modelConstants.getTagsAvailable().toString() + "\n";
		
		help += "Keys-Values available:\n";
		Set keys2 = this.modelConstants.getKeysValuesAvailable().keySet();
		Iterator it2 = keys2.iterator();
		while (it2.hasNext()) {
			String customKey = (String) it2.next();
			
			help += "\t" + customKey + ": " + this.modelConstants.getKeysValuesAvailable().get(customKey) + "\n";
		}
		
		return help;
	}
	
	
	// the function to build a nested annotation, ie an annotation in an annotation
	/**
	 * Get a Metadata object linked to a qualifier used in the parent Metadata
	 * Permits to annotate the qualifier content like you would annotate a component (model, node, transition...)
	 *
	 * @param termDesired the qualifier one wants to retrieve
	 */	
	public Metadata getMetadataOfQualifier(String termDesired) {

		if (this.listOfAnnotations.containsKey(termDesired)) {
			Index indexParent = this.getLocalIndex();
			
			Index index = this.listOfAnnotations.get(termDesired).getIndex(modelConstants, indexParent);
			
			// if the index is null to that point, that means the annotation required doesn't exist and we return null
			// else we return the metadata of this annotation
			if (index != null) {
				return this.modelConstants.getInstanceOfListMetadata().getMetadata(index);
			}
			
			return null;
		}
		else {
			System.out.println("This qualifier doesn't exist, so there can be no metadata object attached to it." + "\n");
			
			return null;
		}
	}
}
