package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.AnnotationFactory;
import org.colomoto.biolqm.metadata.annotations.JsonReader;
import org.colomoto.biolqm.metadata.annotations.DateAnnotation;

import org.colomoto.biolqm.metadata.constants.ModelConstants;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.QualifiersAvailable;
import org.colomoto.biolqm.metadata.constants.TagsKeysAvailable;
import org.colomoto.biolqm.metadata.constants.Index;
import org.colomoto.biolqm.metadata.constants.Qualifier;

import org.colomoto.biolqm.metadata.validations.DateValidator;

import org.json.JSONObject;
import org.json.JSONArray;

import org.sbml.jsbml.CVTerm;
import org.sbml.jsbml.History;
import org.sbml.jsbml.Creator;
import org.sbml.jsbml.xml.XMLNode;
import org.sbml.jsbml.xml.XMLTriple;
import org.sbml.jsbml.xml.XMLAttributes;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.time.LocalDate;
import java.util.ArrayList;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.text.ParseException;
import java.util.stream.Collectors;
import java.util.AbstractMap;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

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
	private Map<String, ArrayList<Annotation>> listOfAnnotations;
	
	private ModelConstants modelConstants;
	
	
	// constructors
	public Metadata(ModelConstants newModelConstants, String newType)
	{
		this.type = newType;
		this.notes = "";
		this.listOfAnnotations = new HashMap<String, ArrayList<Annotation>>();
		
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
	public String getType() {
		return this.type;
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
	
	private boolean isValidURI(String collection, String identifier) {
		
		// check the internet connection
	    try {
			URL url = new URL("http://www.google.com");
			URLConnection connection = url.openConnection();
			connection.connect();
		} catch (MalformedURLException e) {
			System.out.println("Internet is not connected: the uri will be added without validation against the entries of identifiers.org." + "\n");
			return true;
		} catch (IOException e) {
			System.out.println("Internet is not connected: the uri will be added without validation against the entries of identifiers.org." + "\n");
			return true;
		}
		
		String compactId = collection+":"+identifier;
		String fullCompactId = "https://resolver.api.identifiers.org/"+compactId;
		
		try {
			JSONObject jsonURI = JsonReader.readJsonFromUrl(fullCompactId);
			
			if (jsonURI.has("errorMessage") && jsonURI.isNull("errorMessage")) {
				return true;
			}
			else {
				System.err.println("The URI is not valid: it should be a valid entry for identifiers.org." + "\n");
			}
		} catch (IOException e) {
			System.err.println("Error checking the uri: it should be a valid entry for identifiers.org.." + "\n");
		}
		return false;
	}
	
	
	// the functions to manage the alternatives
	private boolean createQualifier(String termDesired, String javaClassDesired) {

		// we find the javaClass dedicated for this qualifier
		String javaClass = this.suitedJavaClass(termDesired);
		
		// if the qualifier doesn't exist yet we add it to the list of qualifiers available
		if (javaClass == null) {
			this.modelConstants.getInstanceOfQualifiersAvailable().updateQualifiers(this.type, termDesired, "customQualifier", javaClassDesired);
			
			javaClass = javaClassDesired;
		}
		
		// if the javaClass for this qualifier match the javaClass asked by the user we create the annotation
		if (javaClass.equals(javaClassDesired)) {
			AnnotationFactory factory = new AnnotationFactory();
			Annotation newAnnotation = factory.getInstance(javaClass);
			
			this.listOfAnnotations.put(termDesired, new ArrayList<Annotation>());
			this.listOfAnnotations.get(termDesired).add(newAnnotation);
			
			return true;
		}
		// else we print a warning
		else {
			System.err.println("You cannot create this type of annotation for this qualifier." + "\n");
			
			return false;
		}
	}
	
	/**
	 * Create a new alternative for a qualifier
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @return the number of the alternative created
	 */	
	public int createAlternative(String termDesired) {
		
		if (this.listOfAnnotations.containsKey(termDesired)) {
			String fullJavaClass = this.getClassOfQualifier(termDesired);
			int colon = fullJavaClass.lastIndexOf('.');
			String javaClass = fullJavaClass.substring(colon+1);
			
			AnnotationFactory factory = new AnnotationFactory();
			Annotation newAnnotation = factory.getInstance(javaClass);
			
			this.listOfAnnotations.get(termDesired).add(newAnnotation);
			
			return this.listOfAnnotations.get(termDesired).size()-1;
		}
		else {
			System.err.println("You have to create this qualifier before creating an alternative." + "\n");
			
			return -1;
		}
	}


	// the functions to add an annotation
	private void addAnnotation(String termDesired, int alternative, String javaClassDesired, String... contentAnnotation) {
		
		// if it's a new qualifier we create a first alternative
		if (!this.listOfAnnotations.containsKey(termDesired) && alternative == 0) {
			boolean creation = this.createQualifier(termDesired, javaClassDesired);
			if (!creation) {
				return;
			}
		}
		// if the qualifier doesn't exist and the alternative is not 0 there is an issue
		else if (!this.listOfAnnotations.containsKey(termDesired)) {
			System.err.println("You have to create this qualifier with the alternative 0 (or with no alternative which is an implicit 0)." + "\n");
			return;
		}
		else if (!this.suitedJavaClass(termDesired).equals(javaClassDesired)) {
			System.err.println("You cannot add this kind of information to this type of qualifier." + "\n");
			return;
		}
		
		// now if it's an alternative that does exist
		if (alternative >= 0 && alternative < this.listOfAnnotations.get(termDesired).size()) {
				this.listOfAnnotations.get(termDesired).get(alternative).addAnnotation(this.modelConstants, this.type, termDesired, contentAnnotation);
		}
		// else it is an alternative that doesn't exist yet
		else {
			System.err.println("This alternative doesn't exist yet for this qualifier. You have to create it first with createAlternative(qualifier)." + "\n");
		}
	}

	/**
	 * Add a new URI to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param alternative the number of the alternative one wants to modify
	 * @param collection the collection of the uri one wants to create (uniprot, chebi...)
	 * @param identifier the entry one wants to point at in the collection
	 */	
	public void addURI(String termDesired, int alternative, String collection, String identifier) {
		if (!this.isValidURI(collection, identifier)) {
			return;
		}
		
		String javaClassDesired = "GenericAnnotation";
		this.addAnnotation(termDesired, alternative, javaClassDesired, "uri", collection, identifier);
	}
	/**
	 * Add a new URI to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param collection the collection of the uri one wants to create (uniprot, chebi...)
	 * @param identifier the entry one wants to point at in the collection
	 */	
	public void addURI(String termDesired, String collection, String identifier) {
		this.addURI(termDesired, 0, collection, identifier);
	}

	/**
	 * Add a new tag to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param alternative the number of the alternative one wants to modify
	 * @param tag the tag one wants to add
	 */	
	public void addTag(String termDesired, int alternative, String tag) {
		String javaClassDesired = "GenericAnnotation";
		this.addAnnotation(termDesired, alternative, javaClassDesired, "tag", tag);
	}	
	/**
	 * Add a new tag to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param tag the tag one wants to add
	 */	
	public void addTag(String termDesired, String tag) {
		this.addTag(termDesired, 0, tag);
	}
	
	/**
	 * Add a new pair key-value to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param alternative the number of the alternative one wants to modify
	 * @param key the place where one wants to store the value
	 * @param value the value one wants to store
	 */	
	public void addKeyValue(String termDesired, int alternative, String key, String value) {
		String javaClassDesired = "GenericAnnotation";
		this.addAnnotation(termDesired, alternative, javaClassDesired, "keyvalue", key, value);
	}
	/**
	 * Add a new pair key-value to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param key the place where one wants to store the value
	 * @param value the value one wants to store
	 */	
	public void addKeyValue(String termDesired, String key, String value) {
		this.addKeyValue(termDesired, 0, key, value);
	}
	
	/**
	 * Add a new author to the component
	 *
	 * @param termDesired the qualifier one wants to annotate
	 * @param name the name of the author
	 * @param surname the surname of the author
	 * @param email the email of the author (optional: put null if you don't want to define it)
	 * @param organisation the organisation of the author (optional: put null if you don't want to define it)
	 * @param orcid the orcid of the author (optional: put null if you don't want to define it)
	 */	
	public void addAuthor(String termDesired, String name, String surname, String email, String organisation, String orcid) {
		if (name == null || surname == null) {
			System.err.println("The name and the surname of the author are compulsory." + "\n");
			return;
		}
		if (!this.isValidEmail(email)) {
			System.err.println("The email is not valid. It should contain an @ (at the very least)." + "\n");
			return;
		}
		if (!this.isValidOrcid(orcid)) {
			System.err.println("The orcid is not valid. It should follow the format ****-****-****-**** with * a number." + "\n");
			return;
		}
		
		String javaClassDesired = "AuthorsAnnotation";
		this.addAnnotation(termDesired, 0, javaClassDesired, name, surname, email, organisation, orcid);
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
			this.addAnnotation(termDesired, 0, javaClassDesired, date);
		}
		else {
			System.err.println("The date is not valid. It should follow the format YYYY-MM-DD." + "\n");
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
		this.addAnnotation(termDesired, 0, javaClassDesired, distribution);
	}
	
	
	// the functions to remove an annotation
	private void removeAnnotation(String termDesired, int alternative, String javaClassDesired, String... contentAnnotation) {
		
		if (!this.listOfAnnotations.containsKey(termDesired)) {
			System.err.println("This type of qualifier has not been defined for this component." + "\n");
		}
		else if (alternative >= 0 && alternative < this.listOfAnnotations.get(termDesired).size()) {
			// we find the javaClass dedicated for this qualifier
			String javaClass = this.suitedJavaClass(termDesired);
			
			if (javaClass == null) {
				System.err.println("This qualifier doesn't exist." + "\n");
			}
			else if (javaClass.equals(javaClassDesired)) {		
				this.listOfAnnotations.get(termDesired).get(alternative).removeAnnotation(this.modelConstants, contentAnnotation);
			}
			else {
				System.err.println("You cannot remove this kind of annotation for this qualifier." + "\n");
			}
		}
		else {
			System.err.println("This alternative doesn't exist yet for this qualifier. You have to create it first with createAlternative(qualifier)." + "\n");
		}
	}
	
	/**
	 * Remove an URI from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param alternative the number of the alternative one wants to modify
	 * @param collection the collection of the uri one wants to remove
	 * @param identifier the entry in the collection one wants to remove
	 */	
	public void removeURI(String termDesired, int alternative, String collection, String identifier) {
		String javaClassDesired = "GenericAnnotation";
		this.removeAnnotation(termDesired, alternative, javaClassDesired, "uri", collection, identifier);
	}
	/**
	 * Remove an URI from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param collection the collection of the uri one wants to remove
	 * @param identifier the entry in the collection one wants to remove
	 */	
	public void removeURI(String termDesired, String collection, String identifier) {
		this.removeURI(termDesired, 0, collection, identifier);
	}
	
	/**
	 * Remove a tag from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param alternative the number of the alternative one wants to modify
	 * @param tag the tag one wants to remove
	 */	
	public void removeTag(String termDesired, int alternative, String tag) {
		String javaClassDesired = "GenericAnnotation";
		this.removeAnnotation(termDesired, alternative, javaClassDesired, "tag", tag);
	}
	/**
	 * Remove a tag from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param tag the tag one wants to remove
	 */	
	public void removeTag(String termDesired, String tag) {
		this.removeTag(termDesired, 0, tag);
	}
	
	/**
	 * Remove a pair key-value from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param alternative the number of the alternative one wants to modify
	 * @param key the place where one wants to remove a value
	 * @param value the value one wants to remove
	 */	
	public void removeKeyValue(String termDesired, int alternative, String key, String value) {
		String javaClassDesired = "GenericAnnotation";
		this.removeAnnotation(termDesired, alternative, javaClassDesired, "keyvalue", key, value);
	}
	/**
	 * Remove a pair key-value from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param key the place where one wants to remove a value
	 * @param value the value one wants to remove
	 */	
	public void removeKeyValue(String termDesired, String key, String value) {
		this.removeKeyValue(termDesired, 0, key, value);
	}
	
	/**
	 * Remove an author from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 * @param name the name of the author
	 * @param surname the surname of the author
	 * @param email the email of the author (optional: put null if you don't want to define it)
	 * @param organisation the organisation of the author (optional: put null if you don't want to define it)
	 * @param orcid the orcid of the author (optional: put null if you don't want to define it)
	 */	
	public void removeAuthor(String termDesired, String name, String surname, String email, String organisation, String orcid) {
		String javaClassDesired = "AuthorsAnnotation";
		this.removeAnnotation(termDesired, 0, javaClassDesired, name, surname, email, organisation, orcid);
	}
	
	/**
	 * Remove a date from the component
	 *
	 * @param termDesired the qualifier one wants to remove
	 */	
	public void removeDate(String termDesired) {
		String javaClassDesired = "DateAnnotation";
		this.removeAnnotation(termDesired, 0, javaClassDesired, "");
	}
	
	/**
	 * Remove the terms of distribution from the component
	 * 
	 * @param termDesired the qualifier one wants to remove
	 */	
	public void removeDistribution(String termDesired) {
		String javaClassDesired = "DistributionAnnotation";
		this.removeAnnotation(termDesired, 0, javaClassDesired, "");
	}
	
	
	// the functions to get a description of the components' annotations
	protected String getDescriptionNestedMetadata(String tab) {
		String description = "";
		
		Set keys = this.listOfAnnotations.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()) {
			String termDesired = (String) it.next();
			description += commonDescriptionAnnotation(termDesired, true, tab);
		}
		
		return description;
	}
	
	private String commonDescriptionAnnotation(String termDesired, boolean nested, String tab) {
		String description = "";
		
		for (int alternative = 0; alternative < this.listOfAnnotations.get(termDesired).size(); alternative++) {
			description += tab + termDesired + " (alternative " + alternative + ")";
			
			String descriptionNested = "";
			if (this.isSetMetadataOfQualifier(termDesired, alternative)) {
				
				if (nested) {
					descriptionNested = tab + "\tNested :\n";
					Metadata nestedMetadata = this.getMetadataOfQualifier(termDesired, alternative);
					if (nestedMetadata != null) {
						descriptionNested += nestedMetadata.getDescriptionNestedMetadata(tab + "\t\t");
					}
				}
				else {
					description += " (nested)";
				}
			}
			
			description += this.listOfAnnotations.get(termDesired).get(alternative).getValue(tab);
			
			description += descriptionNested;
		}
		
		return description;
	}
	
	/**
	 * Retrieve a String containing the description of an annotation
	 * 
	 * @param termDesired the qualifier one wants to get the description of
	 * @param nested a boolean to precise if you want to describe the nested annotations (true for yes, false for no)
	 */	
	public String getDescriptionAnnotation(String termDesired, boolean nested) {
		String description = "";
		
		if (this.listOfAnnotations.containsKey(termDesired)) {
			description += commonDescriptionAnnotation(termDesired, nested, "");
			return description;
		}
		return "This type of qualifier has not been defined for this component" + "\n";
	}
	/**
	 * Retrieve a String containing the description of an annotation (without the nested parts)
	 * 
	 */	
	public String getDescriptionAnnotation(String termDesired) {
		return this.getDescriptionAnnotation(termDesired, false);
	}

	/**
	 * Retrieve a String containing the description of all the component's annotations
	 * 
	 * @param nested a boolean to precise if you want to describe the nested annotations (true for yes, false for no)
	 */	
	public String getDescriptionMetadata(boolean nested) {
		String description = "";
		
		Set keys = this.listOfAnnotations.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()) {
			String termDesired = (String) it.next();
			description += commonDescriptionAnnotation(termDesired, nested, "");
		}
		
		return description;
	}
	/**
	 * Retrieve a String containing the description of all the component's annotations (without the nested parts)
	 * 
	 */	
	public String getDescriptionMetadata() {
		return this.getDescriptionMetadata(false);
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
			
			if (termDesired == "isDescribedBy") {
				help += "DOIs already used in the model:\n" + this.modelConstants.getInstanceOfExternalMetadata().getDescription();
			}
		
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
			
			if (term.equals("isDescribedBy")) {
				help += "Extra: DOIs already used in the model:\n" + this.modelConstants.getInstanceOfExternalMetadata().getDescription();
			}
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
	
	
	// the functions to manage a nested annotation, ie an annotation in an annotation
	/**
	 * Check if a metadata object exists for a qualifier
	 *
	 * @param termDesired the name of the qualifier you want to check
	 * @param alternative the number of the alternative you want to check
	 * @return true if it exists, false otherwise
	 */	
	public boolean isSetMetadataOfQualifier(String termDesired, int alternative) {

		if (this.listOfAnnotations.containsKey(termDesired) && alternative >= 0 && alternative < this.listOfAnnotations.get(termDesired).size()) {
			Index indexParent = this.getLocalIndex();
			
			return this.listOfAnnotations.get(termDesired).get(alternative).isSetIndex(modelConstants, indexParent);
		}
		return false;
	}
	/**
	 * Check if a metadata object exists for a qualifier
	 *
	 * @param termDesired the name of the qualifier you want to check
	 * @return true if it exists, false otherwise
	 */	
	public boolean isSetMetadataOfQualifier(String termDesired) {

		return this.isSetMetadataOfQualifier(termDesired, 0);
	}
	
	/**
	 * Get the Metadata object associated to a given qualifier used in the parent Metadata
	 * The Metadata returned constitutes a nested annotation for the parent Metadata
	 * It can be modified exactly as the parent Metadata
	 *
	 * @param termDesired the qualifier one wants to retrieve
	 * @param alternative the number of the alternative you want to check
	 */	
	public Metadata getMetadataOfQualifier(String termDesired, int alternative) {

		if (!this.listOfAnnotations.containsKey(termDesired)) {
			boolean creation = this.createQualifier(termDesired, "GenericAnnotation");
			if (!creation) {
				return null;
			}
		}
		
		if (alternative >= 0 && alternative < this.listOfAnnotations.get(termDesired).size()) {
			Index indexParent = this.getLocalIndex();
			
			Index index = this.listOfAnnotations.get(termDesired).get(alternative).getIndex(modelConstants, indexParent);
			
			// if the index is null to that point, that means the annotation required doesn't exist and we return null
			// else we return the metadata of this annotation
			if (index != null) {
				return this.modelConstants.getInstanceOfListMetadata().getMetadata(index);
			}
			return null;
		}
		else {
			System.err.println("This alternative doesn't exist yet for this qualifier. You have to create it first with createAlternative(qualifier)." + "\n");
			return null;
		}
	}
	/**
	 * Get the Metadata object associated to a given qualifier used in the parent Metadata
	 * The Metadata returned constitutes a nested annotation for the parent Metadata
	 * It can be modified exactly as the parent Metadata
	 *
	 * @param termDesired the qualifier one wants to retrieve
	 */	
	public Metadata getMetadataOfQualifier(String termDesired) {

		return this.getMetadataOfQualifier(termDesired, 0);
	}
	
	
	// the functions used to import/export the metadata towards the sbml format or the json format
	private Set<String> getListOfQualifiers() {
		return this.listOfAnnotations.keySet();
	}

	private String getClassOfQualifier(String qualifierName) {
		return this.listOfAnnotations.get(qualifierName).get(0).getClass().getName();
	}
	
	private ArrayList<ArrayList<String>> getResourcesOfQualifier(String qualifierName, int alternative) {
		return this.listOfAnnotations.get(qualifierName).get(alternative).getResources();
	}

	/**
	 * Check if the metadata object is empty or contains annotation
	 *
	 * @return false if the metadata object is empty, true otherwise
	 */	
	public boolean isMetadataNotEmpty() {
		if (this.listOfAnnotations.size() > 0) {
			return true;
		}
		return false;
	}
	
	/**
	 * Get the number of alternatives for a given qualifier
	 *
	 * @param qualifierName the name of the qualifier you're interested in
	 * @return the number of alternatives for this qualifier
	 */	
	public int getNumberOfAlternatives(String qualifierName) {
		if (!this.listOfAnnotations.containsKey(qualifierName)) {
			return 0;
		}
		return this.listOfAnnotations.get(qualifierName).size();
	}
	
	
	// the functions to export the metadata towards the json format	
	/**
	 * Produces a json expression of the metadata object (internal use)
	 *
	 * @return JSONArray an array of JSONObject, one for each qualifier used in this metadata
	 */
	public JSONArray getJSONOfMetadata() {
		
		JSONArray arrayQualifiers = new JSONArray();
		
		for (String qualifierName: this.getListOfQualifiers()) {
			
			String qualifierFullClass = this.getClassOfQualifier(qualifierName);
			int colon = qualifierFullClass.lastIndexOf('.');
			String qualifierClass = qualifierFullClass.substring(colon+1);
			
			JSONObject jsonQualifier = new JSONObject();
			
			jsonQualifier.put("qualifier", qualifierName);
			jsonQualifier.put("type", qualifierClass);
			
			JSONArray arrayAlternatives = new JSONArray();
			
			for (int alternative = 0; alternative < this.getNumberOfAlternatives(qualifierName); alternative++) {
				
				JSONObject jsonAlternative = this.listOfAnnotations.get(qualifierName).get(alternative).getJSONOfAnnotation();

				// to check if the alternative contains a nested metadata
				if (this.isSetMetadataOfQualifier(qualifierName, alternative)) {
					Metadata metadataNested = this.getMetadataOfQualifier(qualifierName, alternative);
					
					JSONObject jsonNested = new JSONObject();
					
					// if there is some metadata we add the json representation in the json object
					if (metadataNested.isMetadataNotEmpty()) {
						jsonNested.put("annotation", metadataNested.getJSONOfMetadata());
					}
					// if there is some notes we add the json representation in the json object
					if (metadataNested.getNotes() != "") {
						jsonNested.put("notes", metadataNested.getNotes());
					}
					
					if (!jsonNested.isEmpty()) {
						jsonAlternative.put("nested", jsonNested);
					}
				}
				
				arrayAlternatives.put(jsonAlternative);
			}
			
			if (!arrayAlternatives.isEmpty()) {
				jsonQualifier.put("alternatives", arrayAlternatives);
				arrayQualifiers.put(jsonQualifier);
			}
		}
		
		return arrayQualifiers;
	}
	
	
	// the functions to export the metadata towards the sbml format	
	private XMLNode exportTagsAndKeys(int alternative, Set<String> listOfTags, Map<String, ArrayList<String>> listOfKeysValues, XMLNode xmlNested) {

		XMLAttributes attributesAlternative = new XMLAttributes();
		attributesAlternative.add("colomoto:number", String.valueOf(alternative));
		XMLNode xmlAlternative = new XMLNode(new XMLTriple("colomoto:alternative"), attributesAlternative);
		
		if (listOfTags.size() > 0) {
			XMLNode xmlTags = new XMLNode(new XMLTriple("colomoto:tags"));
			
			for (String tag: listOfTags) {
				XMLNode xmlTag = new XMLNode(new XMLTriple("colomoto:tag"));
				xmlTag.addChild(new XMLNode(tag));
				xmlTags.addChild(xmlTag);
			}
			
			xmlAlternative.addChild(xmlTags);
		}
		if (listOfKeysValues.size() > 0) {
			XMLNode xmlKeys = new XMLNode(new XMLTriple("colomoto:keys"));
			
			for (String key : listOfKeysValues.keySet()) {
				XMLAttributes attributesKey = new XMLAttributes();
				attributesKey.add("colomoto:key", String.valueOf(key));
				XMLNode xmlKey = new XMLNode(new XMLTriple("colomoto:values"), attributesKey);
				
				String values = listOfKeysValues.get(key).stream().map(Object::toString).collect(Collectors.joining(";;;"));
				xmlKey.addChild(new XMLNode(values));
				
				xmlKeys.addChild(xmlKey);
			}
			
			xmlAlternative.addChild(xmlKeys);
		}
		if (xmlNested.getChildCount() > 0) {
			xmlAlternative.addChild(xmlNested);
		}
		return xmlAlternative;
	}
	
	private AbstractMap.SimpleEntry<ArrayList<CVTerm>, XMLNode> exportNestedMetadata(Metadata metadata) {
		
		ArrayList<CVTerm> listOfCVTerms = new ArrayList<CVTerm>();
		XMLNode xml = new XMLNode(new XMLTriple("colomoto:nested"));
		
		for (String qualifierName: metadata.getListOfQualifiers()) {
			
			String qualifierFullClass = metadata.getClassOfQualifier(qualifierName);
			int colon = qualifierFullClass.lastIndexOf('.');
			String qualifierClass = qualifierFullClass.substring(colon+1);
			
			if (qualifierClass.equals("GenericAnnotation")) {
				AbstractMap.SimpleEntry<ArrayList<CVTerm>, XMLNode> commonExportMetadata = this.commonExportMetadata(metadata, xml, qualifierName, qualifierClass);
				
				for (CVTerm cvterm: commonExportMetadata.getKey()) {
					listOfCVTerms.add(cvterm);
				}
				xml = commonExportMetadata.getValue();
			}
		}
		
		return new AbstractMap.SimpleEntry<ArrayList<CVTerm>, XMLNode>(listOfCVTerms, xml);
	}
	
	private AbstractMap.SimpleEntry<ArrayList<CVTerm>, XMLNode> commonExportMetadata(Metadata metadata, XMLNode xml, String qualifierName, String qualifierClass) {
		
		ArrayList<CVTerm> listOfCVTerms = new ArrayList<CVTerm>();
		
		// XMLNode for the tags and keys of the qualifier
		XMLAttributes attributesQualifier = new XMLAttributes();
		attributesQualifier.add("colomoto:name", qualifierName);
		attributesQualifier.add("colomoto:type", qualifierClass);
		XMLNode xmlQualifier = new XMLNode(new XMLTriple("colomoto:qualifier"), attributesQualifier);
		
		boolean tagsorkeys = false;
		
		int alternativeWithoutResources = 0;
		for (int alternative = 0; alternative < metadata.getNumberOfAlternatives(qualifierName); alternative++) {
			CVTerm cvterm = new CVTerm();
			
			org.sbml.jsbml.CVTerm.Qualifier qualifier;
			if (metadata.getType().equals("model")) {
				qualifier = CVTerm.Qualifier.getModelQualifierFor(qualifierName);
			}
			else {
				qualifier = CVTerm.Qualifier.getBiologicalQualifierFor(qualifierName);
			}
			cvterm.setQualifier(qualifier);
			
			if (qualifier.getElementNameEquivalent().equals("unknownQualifier") || (qualifier.getElementNameEquivalent().equals("isRelatedTo") && !qualifierName.equals("isRelatedTo"))) {
				cvterm.setUnknownQualifierName(qualifierName);
			}
		
			ArrayList<ArrayList<String>> listOfResources = metadata.getResourcesOfQualifier(qualifierName, alternative);

			for (ArrayList<String> resource: listOfResources) {
				cvterm.addResource(resource.get(0)+":"+resource.get(1));
			}
			
			// we save the nested parts
			XMLNode xmlNested = new XMLNode();
			if (metadata.isSetMetadataOfQualifier(qualifierName, alternative)) {
				Metadata metadataQualifier = metadata.getMetadataOfQualifier(qualifierName, alternative);
				
				AbstractMap.SimpleEntry<ArrayList<CVTerm>, XMLNode> nestedMetadata = metadata.exportNestedMetadata(metadataQualifier);
				
				ArrayList<CVTerm> nestedCVTerms = nestedMetadata.getKey();
				for (CVTerm nestedCVTerm : nestedCVTerms) {
					cvterm.addNestedCVTerm(nestedCVTerm);
				}
				xmlNested = nestedMetadata.getValue();
			}
			
			int realAlternative;
			if (cvterm.getResourceCount() > 0 || cvterm.getNestedCVTermCount() > 0) {
				listOfCVTerms.add(cvterm);
				realAlternative = alternative-alternativeWithoutResources;
			}
			else {
				alternativeWithoutResources += 1;
				realAlternative = -1;
			}
			
			// we save the tags and keysvalues in an XMLNode aside
			GenericAnnotation generic = (GenericAnnotation) metadata.listOfAnnotations.get(qualifierName).get(alternative);
			Set<String> listOfTags = generic.getListOfTags();
			Map<String, ArrayList<String>> listOfKeysValues = generic.getListOfKeysValues();
			if (listOfTags.size() > 0 || listOfKeysValues.size() > 0 || xmlNested.getChildCount() > 0) {
				tagsorkeys = true;
				xmlQualifier.addChild(metadata.exportTagsAndKeys(realAlternative, listOfTags, listOfKeysValues, xmlNested));
			}
		}
		
		if (tagsorkeys) {
			xml.addChild(xmlQualifier);
		}
		
		return new AbstractMap.SimpleEntry<ArrayList<CVTerm>, XMLNode>(listOfCVTerms, xml);
	}
	
	/**
	 * Produces a jSBML expression of the metadata object (internal use)
	 *
	 * @return org.sbml.jsbml.Annotation all the information contained in the metadata minus some details that are not acceptted in SBML
	 */
	public org.sbml.jsbml.Annotation getSBMLOfMetadata() {
		
		org.sbml.jsbml.Annotation annotation = new org.sbml.jsbml.Annotation();
		
		History history = new History();
		
		XMLAttributes attributesXml = new XMLAttributes();
		attributesXml.add("xmlns:colomoto", "uri_colomoto");
		
		XMLNode xml = new XMLNode(new XMLTriple("colomoto:nonRDFAnnotation"), attributesXml);
		
		for (String qualifierName: this.getListOfQualifiers()) {
			
			String qualifierFullClass = this.getClassOfQualifier(qualifierName);
			int colon = qualifierFullClass.lastIndexOf('.');
			String qualifierClass = qualifierFullClass.substring(colon+1);
			
			if (qualifierClass.equals("GenericAnnotation")) {
				AbstractMap.SimpleEntry<ArrayList<CVTerm>, XMLNode> commonExportMetadata = this.commonExportMetadata(this, xml, qualifierName, qualifierClass);
				
				for (CVTerm cvterm: commonExportMetadata.getKey()) {
					annotation.addCVTerm(cvterm);
				}
				xml = commonExportMetadata.getValue();
			}
			else if (qualifierClass.equals("AuthorsAnnotation")) {
				ArrayList<ArrayList<String>> listOfAuthors = this.getResourcesOfQualifier(qualifierName, 0);
				for (ArrayList<String> author: listOfAuthors) {
					Creator creator = new Creator();
					creator.setGivenName(author.get(0));
					creator.setFamilyName(author.get(1));
					if (author.get(2) != null) { creator.setOrganisation(author.get(2)); }
					if (author.get(3) != null) { creator.setEmail(author.get(3)); }
					//if (author.get(4) != null) { creator.setOtherAttribute("orcid", author.get(4)); }
					history.addCreator(creator);
				}
			}
			else if (qualifierClass.equals("DateAnnotation")) {
				String date = this.getResourcesOfQualifier(qualifierName, 0).get(0).get(0);
				
				String pattern = "yyyy-MM-dd";
				try {
					Date simpleDateFormat = new SimpleDateFormat(pattern).parse(date);
				
					if (qualifierName == "created") {
						history.setCreatedDate(simpleDateFormat);
					}
					else if (qualifierName == "modified") {
						history.setModifiedDate(simpleDateFormat);
					}
				} catch (ParseException e) {
					System.err.println("Error parsing a date contained in an annotation of the model." + "\n");
				}
			}
		}
		
		annotation.setHistory(history);
		if (xml.getChildCount() > 0) {
			annotation.setNonRDFAnnotation(xml);
		}
		
		return annotation;
	}
	
	
	// the functions to import a json file of annotations as the annotations
	private int doesAlternativeExist(JSONObject jsonAlternative, String termDesired) {
		
		if (this.listOfAnnotations.containsKey(termDesired)) {
			
			for(int idAlt = 0; idAlt < this.getNumberOfAlternatives(termDesired); idAlt++) {
				
				boolean exist = this.listOfAnnotations.get(termDesired).get(idAlt).doesAlternativeExist(jsonAlternative);				
				if (exist == true) {
					return idAlt;
				}
			}
		}
		return -1;
	}
	
	private boolean equalsMetadata(JSONObject json) {
		
		if (json.has("annotation") && !json.isNull("annotation")) {
			
			JSONArray arrayQualifiers = json.getJSONArray("annotation");
			for(int idQualifier = 0; idQualifier < arrayQualifiers.length(); idQualifier++)
			{
				JSONObject jsonQualifier = arrayQualifiers.getJSONObject(idQualifier);
				
				String qualifierName = jsonQualifier.getString("qualifier");
				String qualifierClass = jsonQualifier.getString("type");
				
				JSONArray arrayAlternatives = jsonQualifier.getJSONArray("alternatives");

				if (qualifierClass.equals("GenericAnnotation")) {
					
					int numAltJson = arrayAlternatives.length();
					for(int idAlternative = 0; idAlternative < numAltJson; idAlternative++)
					{
						JSONObject jsonAlternative = arrayAlternatives.getJSONObject(idAlternative);

						int numSameAlt = this.doesAlternativeExist(jsonAlternative, qualifierName);
						
						if (numSameAlt != -1) {
							
							if ((jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) && this.isSetMetadataOfQualifier(qualifierName, numSameAlt)) {
								Metadata nestedMetadata = this.getMetadataOfQualifier(qualifierName, numSameAlt);
								JSONObject nestedJson = jsonAlternative.getJSONObject("nested");
								
								if (!nestedMetadata.equalsMetadata(nestedJson)) {
									return false;
								}
							}
							else if ((jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) || this.isSetMetadataOfQualifier(qualifierName, numSameAlt)) {
								return false;
							}
						}
						else {
							return false;
						}
					}
				}
				else {
					JSONObject jsonAlternative = arrayAlternatives.getJSONObject(0);
					
					int numSameAlt = this.doesAlternativeExist(jsonAlternative, qualifierName);
					
					if (numSameAlt == -1) {
						return false;
					}
					
					if ((jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) && this.isSetMetadataOfQualifier(qualifierName)) {
						
						Metadata nestedMetadata = this.getMetadataOfQualifier(qualifierName);
						JSONObject nestedJson = jsonAlternative.getJSONObject("nested");
						
						if (!nestedMetadata.equalsMetadata(nestedJson)) {
							return false;
						}
					}
					else if (!(jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) && !this.isSetMetadataOfQualifier(qualifierName)) {
						
					}
					else {
						return false;
					}
				}
			}
		}
		
		String existingNotes = this.getNotes();
		if (json.has("notes") && !json.isNull("notes") && !existingNotes.equals(json.get("notes"))) {
			return false;
		}
		
		return true;
	}
	
	/**
	 * Permits to import a json file to extend the existent annotations (internal use)
	 *
	 * @return org.sbml.jsbml.Annotation all the information contained in the metadata minus some details that are not acceptted in SBML
	 */
	public void importElementMetadata(JSONObject json) {
		
		// if there is some metadata we add the json representation in the json object
		if (json.has("annotation") && !json.isNull("annotation")) {
			
			JSONArray arrayQualifiers = json.getJSONArray("annotation");
			for(int idQualifier = 0; idQualifier < arrayQualifiers.length(); idQualifier++)
			{
				JSONObject jsonQualifier = arrayQualifiers.getJSONObject(idQualifier);
				
				String qualifierName = jsonQualifier.getString("qualifier");
				String qualifierClass = jsonQualifier.getString("type");
				
				JSONArray arrayAlternatives = jsonQualifier.getJSONArray("alternatives");

				if (qualifierClass.equals("GenericAnnotation")) {
					
					int numAltMetadata = this.getNumberOfAlternatives(qualifierName);
					int numAltJson = arrayAlternatives.length();
					boolean alternativesExist = false;
					if (numAltMetadata > 1 || numAltJson > 1) {
						alternativesExist = true;
					}
					
					for(int idAlternative = 0; idAlternative < numAltJson; idAlternative++)
					{
						JSONObject jsonAlternative = arrayAlternatives.getJSONObject(idAlternative);
						
						int numberAlternative = 0;
						if (alternativesExist) {
							
							int numSameAlt = this.doesAlternativeExist(jsonAlternative, qualifierName);
							if (numSameAlt != -1) {
								
								if ((jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) && this.isSetMetadataOfQualifier(qualifierName, numSameAlt)) {
									Metadata nestedMetadata = this.getMetadataOfQualifier(qualifierName, numSameAlt);
									JSONObject nestedJson = jsonAlternative.getJSONObject("nested");
									
									if (nestedMetadata.equalsMetadata(nestedJson)) {
										numberAlternative = -1;
									}
									else {
										numberAlternative = this.createAlternative(qualifierName);
									}
								}
								else if (!(jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) && !this.isSetMetadataOfQualifier(qualifierName, numSameAlt)) {
									numberAlternative = -1;
								}
								else {
									numberAlternative = this.createAlternative(qualifierName);
								}
							}
							else if (numAltMetadata != 0) {
								numberAlternative = this.createAlternative(qualifierName);
							}
						}
						
						if (numberAlternative != -1) {
							if (jsonAlternative.has("uris") && !jsonAlternative.isNull("uris")) {
								JSONArray arrayURIs = jsonAlternative.getJSONArray("uris");
								for(int idUri = 0; idUri < arrayURIs.length(); idUri++)
								{
									JSONObject jsonURI = arrayURIs.getJSONObject(idUri);
									this.addURI(qualifierName, numberAlternative, jsonURI.getString("collection"), jsonURI.getString("identifier"));
								}
							}
							if (jsonAlternative.has("tags") && !jsonAlternative.isNull("tags")) {
								JSONArray arrayTags = jsonAlternative.getJSONArray("tags");
								for(int idTag = 0; idTag < arrayTags.length(); idTag++)
								{
									String tag = arrayTags.getString(idTag);
									this.addTag(qualifierName, numberAlternative, tag);
								}
							}
							if (jsonAlternative.has("keysvalues") && !jsonAlternative.isNull("keysvalues")) {
								JSONArray arrayKeys = jsonAlternative.getJSONArray("keysvalues");
								for(int idKey = 0; idKey < arrayKeys.length(); idKey++)
								{
									JSONObject key = arrayKeys.getJSONObject(idKey);
									JSONArray arrayValues = key.getJSONArray("values");
									
									for (int idValue = 0; idValue < arrayValues.length(); idValue++) {
										this.addKeyValue(qualifierName, numberAlternative, key.getString("key"), arrayValues.getString(idValue));
									}
								}
							}
								
							if (jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) {
								
								Metadata metadataQualifier = this.getMetadataOfQualifier(qualifierName, numberAlternative);
								
								metadataQualifier.importElementMetadata(jsonAlternative.getJSONObject("nested"));
							}
						}
					}
				}
				else {
					JSONObject jsonAlternative = arrayAlternatives.getJSONObject(0);
					
					if (qualifierClass.equals("AuthorsAnnotation")) {
						
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
							
							this.addAuthor(qualifierName, author.getString("name"), author.getString("surname"), email, organisation, orcid);
						}
					}
					else if (qualifierClass.equals("DateAnnotation")) {
						
						if (qualifierName.equals("created") && this.listOfAnnotations.containsKey(qualifierName)) {
							DateAnnotation dateAnnotation = (DateAnnotation) this.listOfAnnotations.get(qualifierName).get(0);
							
							SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
							try {
								Date date = sdf.parse(dateAnnotation.getDate());
								Date dateJSON = sdf.parse(jsonAlternative.getString("date"));
								
								if (dateJSON.before(date)) {
									this.addDate(qualifierName, jsonAlternative.getString("date"));
								}
							} catch (ParseException e) {
								System.err.println("Error parsing a date contained in an annotation of the model." + "\n");
							}
						}
						this.addDate(qualifierName, jsonAlternative.getString("date"));
					}
					else if (qualifierClass.equals("DistributionAnnotation")) {

						this.addDistribution(qualifierName, jsonAlternative.getString("distribution"));
					}
					
					if (jsonAlternative.has("nested") && !jsonAlternative.isNull("nested")) {
						
						Metadata metadataQualifier = this.getMetadataOfQualifier(qualifierName);
						
						metadataQualifier.importElementMetadata(jsonAlternative.getJSONObject("nested"));
					}
				}
			}
		}
		// if there is some notes we add the json representation in the json object
		if (json.has("notes") && !json.isNull("notes")) {
			
			String existingNotes = this.getNotes();
			this.setNotes(existingNotes + json.get("notes"));
		}
	}
}
