package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.constants.Qualifier;

import org.yaml.snakeyaml.Yaml;

import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.io.InputStream;

/**
 * One instance per model opened to store the different types of qualifiers available for each component
 * When a URI involving a new collection is added, the list of collections useful for the relevant qualifier is updated
 *
 * @author Martin Boutroux
 */
public class QualifiersAvailable {
	
	// variables
	public Map<String, Qualifier> model;
	public Map<String, Qualifier> species;
	public Map<String, Qualifier> transition;
	public Map<String, Qualifier> nested;
	
	// constructors
	public QualifiersAvailable() {
		this.model = new HashMap<String, Qualifier>();
		this.species = new HashMap<String, Qualifier>();
		this.transition = new HashMap<String, Qualifier>();
		this.nested = new HashMap<String, Qualifier>();
	
		Yaml yaml = new Yaml();
		InputStream inputStream = QualifiersAvailable.class
		  .getClassLoader()
		  .getResourceAsStream("suggestions.yaml");
		  
		Map<String, Object> obj = yaml.load(inputStream);
		
		for (String ontology : obj.keySet()) {
			String newOntology = ontology;
			ArrayList<Map<String, Object>> termsOntology = (ArrayList<Map<String, Object>>) obj.get(ontology);
			
			for (Map<String, Object> caracs : termsOntology) {
				String newTerm = (String) caracs.get("term");
				String newDescription = (String) caracs.get("description");
				String newDefinition = (String) caracs.get("definition");
				
				String newJavaClass;
				if (caracs.containsKey("javaClass")) {
					newJavaClass = (String) caracs.get("javaClass");
				}
				else {
					newJavaClass = "GenericAnnotation";
				}
				
				Map<String, Object> caracAppearance = (Map<String, Object>) caracs.get("appears");
				for (String component : caracAppearance.keySet()) {
					Map<String, Object> caracsComponent = (HashMap<String, Object>) caracAppearance.get(component);
					
					String newProbability = (String) caracsComponent.get("probability");
					ArrayList<String> newCollections = (ArrayList<String>) caracsComponent.get("collections");
					
					Qualifier newQualifier = new Qualifier(newOntology, newDescription, newDefinition, newJavaClass, newProbability, newCollections);
					
					this.selectionVariable(component).put(newTerm, newQualifier);
				}
			}
		}
	}
	
	// functions
	public Map<String, Qualifier> selectionVariable(String component) {
		switch (component) {
			case "model":
				return this.model;
			case "species":
				return this.species;
			case "transition":
				return this.transition;
			case "nested":
				return this.nested;
		}
		
		return null;
	}
	
	public void updateCollections(String component, String termDesired, String collection) {
		Map<String, Qualifier> componentVariable = this.selectionVariable(component);
		
		componentVariable.get(termDesired).setCollections(collection);
	}
	
	public void updateQualifiers(String component, String termDesired, String description, String javaClass) {
		Map<String, Qualifier> componentVariable = this.selectionVariable(component);
		
		componentVariable.put(termDesired, new Qualifier(description, javaClass));
	}
	
	public String getParametersQualifier(String component, String termDesired) {
		Map<String, Qualifier> componentVariable = this.selectionVariable(component);
		
		String javaClassDesired = componentVariable.get(termDesired).getJavaClass();
		
		String help = "You can manipulate this qualifier with the methods add<Type>(qualifier, parameters) and remove<Type>(qualifier parameters) on the component annotated.\n";
		
		switch (javaClassDesired) {
			case "GenericAnnotation":
				help += "You can create several alternatives for a given qualifier in a component with createAlternative().\n";
				help += "It returns the number of the alternative you created.\n";
				help += "This qualifier accepts URIs, Tags, Keys-Values. To create them you use :\n";
				help += "- addURI(the qualifier, the alternative (optional, 0 if not defined), the collection, its identifier)\n";
				help += "- addTag(the qualifier, the alternative (optional, 0 if not defined), the tag)\n";
				help += "- addKeyValue(the qualifier, the alternative (optional, 0 if not defined), the key, the value)\n";
				help += "To remove them you use :\n";
				help += "- removeURI(the qualifier, the alternative (optional, 0 if not defined), the collection, its identifier)\n";
				help += "- removeTag(the qualifier, the alternative (optional, 0 if not defined), the tag)\n";
				help += "- removeKeyValue(the qualifier, the alternative (optional, 0 if not defined), the key, the value)\n";
				help += "You can also start a nested annotation in this qualifier with getMetadataOfQualifier(the qualifier, the alternative (optional, 0 if not defined)).\n";
				help += "It returns the Metadata object corresponding to the nested annotation.\n";
				break;
			case "AuthorsAnnotation":
				help += "This qualifier accepts Authors. To create them you use :\n";
				help += "- addAuthor(the qualifier, the name of the author, its surname, its email, its organisation, its orcid)\n";
				help += "To remove them you use :\n";
				help += "- removeAuthor(the qualifier, the name of the author, its surname, its email, its organisation, its orcid)\n";
				help += "You can put null in a field if you don't know the value.\n";
				help += "You can also start a nested annotation in this qualifier with getMetadataOfQualifier(the qualifier).\n";
				help += "It returns the Metadata object corresponding to the nested annotation.\n";
				break;
			case "DateAnnotation":
				help += "This qualifier accepts a date. To create it you use :\n";
				help += "- addDate(the qualifier, a date following the standard of the W3C)\n";
				help += "To remove it you use :\n";
				help += "- removeDate(the qualifier, a date following the standard of the W3C)\n";
				help += "You can also start a nested annotation in this qualifier with getMetadataOfQualifier(the qualifier).\n";
				help += "It returns the Metadata object corresponding to the nested annotation.\n";
				break;
			case "DistributionAnnotation":
				help += "This qualifier accepts the distribution terms. To create them you use :\n";
				help += "- addDistribution(the qualifier, the distribution terms)\n";
				help += "To remove them you use :\n";
				help += "- removeDistribution(the qualifier, the distribution terms)\n";
				help += "You can also start a nested annotation in this qualifier with getMetadataOfQualifier(the qualifier).\n";
				help += "It returns the Metadata object corresponding to the nested annotation.\n";
				break;
		}
		
		return help;
	}
	
	public String getHelpQualifier(String component, String termDesired) {
		Map<String, Qualifier> componentVariable = this.selectionVariable(component);
		
		Qualifier qualifierDesired = componentVariable.get(termDesired);
		
		String help = "\tOntology: " + qualifierDesired.getOntology() + "\n" + "\tDescription: " + qualifierDesired.getDescription() + "\n"  + "\tDefinition: " + qualifierDesired.getDefinition() + "\n"  + "\tProbability: " + qualifierDesired.getProbability() + "\n"  + "\tCollections: " + qualifierDesired.getCollections().toString() + "\n";
		
		return help;
	}
}