package org.colomoto.biolqm.metadata.constants;

import java.util.ArrayList;

/**
 * Generic class for the qualifiers
 * This contains the characteristics of a qualifier (its ontology, its description, its definition, its probability to be used, the most useful collections for this qualifier)
 *
 * @author Martin Boutroux
 */
public class Qualifier {
	
	// variables
	private String ontology;
	private String description;
	private String definition;
	private String javaClass;
	private String probability;
	private ArrayList<String> collections;
	
	// constructors
	public Qualifier(String newDescription, String newJavaClass) {
		this.description = newDescription;
		this.javaClass = newJavaClass;
		this.collections = new ArrayList<String>();
	}
	
	public Qualifier(String newOntology, String newDescription, String newDefinition, String newJavaClass, String newProbability, ArrayList<String> newCollections) {
		this.ontology = newOntology;
		this.description = newDescription;
		this.definition = newDefinition;
		this.javaClass = newJavaClass;
		this.probability = newProbability;
		this.collections = newCollections;
	}
	
	// getters
	public String getOntology() {
		return this.ontology;
	}
	public String getDescription() {
		return this.description;
	}
	public String getDefinition() {
		return this.definition;
	}
	public String getJavaClass() {
		return this.javaClass;
	}
	public String getProbability() {
		return this.probability;
	}
	public ArrayList<String> getCollections() {
		return this.collections;
	}
	
	// setters
	public void setCollections(String collection) {
		if (!this.collections.contains(collection)) {
			this.collections.add(collection);
		}
	}
}