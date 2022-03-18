package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.AnnotationTarget;

import java.util.*;

/**
 * One instance per model opened containing the constants relative to the model
 *
 * @author Martin Boutroux
 */
public class ModelConstants {
	
	private final ExternalMetadata instanceOfExternalMetadata;
	private final QualifiersAvailable instanceOfQualifiersAvailable;
	private final CollectionsAvailable instanceOfCollectionsAvailable;

	private final Set<String> allTags, allKeys;

	public ModelConstants() {
		this.instanceOfExternalMetadata = new ExternalMetadata();
		this.instanceOfQualifiersAvailable = new QualifiersAvailable();
		this.instanceOfCollectionsAvailable = new CollectionsAvailable();

		this.allTags = new HashSet<>();
		this.allKeys = new HashSet<>();
	}

	public ExternalMetadata getInstanceOfExternalMetadata() {
		return this.instanceOfExternalMetadata;
	}
	public QualifiersAvailable getInstanceOfQualifiersAvailable() {
		return this.instanceOfQualifiersAvailable;
	}
	public CollectionsAvailable getInstanceOfCollectionsAvailable() {
		return this.instanceOfCollectionsAvailable;
	}

	public Set<String> availableTags() {
		return this.allTags;
	}
	public void addTag(String tag) {
		this.allTags.add(tag);
	}

	public Set<String> availableKeys() {
		return this.allKeys;
	}
	public void addKey(String key) {
		this.allKeys.add(key);
	}


	/**
	 * Retrieve a String containing suggestions on how to annotate the component for a given qualifier
	 *
	 * @param termDesired the qualifier one wants to get some help with
	 */
	public String getHelpAnnotation(AnnotationTarget target, String termDesired) {
		StringBuilder help = new StringBuilder();

//		if (this.getQualifiersAvailable(target).containsKey(termDesired)) {
//			help.append(termDesired).append(":\n");
//
//			help.append(this.getInstanceOfQualifiersAvailable().getParametersQualifier(target, termDesired));
//
//			help.append("Characteristics:\n").append(this.getInstanceOfQualifiersAvailable().getHelpQualifier(target, termDesired));
//
//			if ("isDescribedBy".equals(termDesired)) {
//				help.append("DOIs already used in the model:\n").append(this.getInstanceOfExternalMetadata().getDescription());
//			}
//
//			help.append("Tags available:\n" + "\t").append(this.availableTags().toString()).append("\n");
//			help.append("Keys available:\n" + "\t").append(this.availableKeys().toString()).append("\n");
//		}

		if (help.length() > 0) {
			return help.toString();
		}

		return "This type of qualifier has not been defined for this component" + "\n";
	}

	/**
	 * Retrieve a String containing suggestions on how to annotate the component
	 */
	public String getHelpMetadata(AnnotationTarget target) {
		StringBuilder help = new StringBuilder("Qualifiers available:\n");

//		Map<String, Qualifier> listQualifiersComponent = this.getQualifiersAvailable(target);
//
//		Set<String> keys = listQualifiersComponent.keySet();
//		for (String term : keys) {
//			help.append("-").append(term).append(":\n").append(this.getInstanceOfQualifiersAvailable().getHelpQualifier(target, term));
//
//			if (term.equals("isDescribedBy")) {
//				help.append("Extra: DOIs already used in the model:\n").append(this.getInstanceOfExternalMetadata().getDescription());
//			}
//		}

		help.append("Tags available:\n" + "\t").append(this.availableTags().toString()).append("\n");
		help.append("Keys available:\n" + "\t").append(this.availableKeys().toString()).append("\n");

		return help.toString();
	}

}
