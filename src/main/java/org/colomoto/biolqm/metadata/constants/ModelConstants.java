package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.metadata.annotations.URI;

import java.util.Map;
import java.util.Set;
import java.util.ArrayList;

/**
 * One instance per model opened containing the constants relative to the model
 *
 * @author Martin Boutroux
 */
public class ModelConstants {
	
	// variables
	public ExternalMetadata instanceOfExternalMetadata;
	public ListMetadata instanceOfListMetadata;
	public QualifiersAvailable instanceOfQualifiersAvailable;
	public TagsKeysAvailable instanceOfTagsKeysAvailable;
	public CollectionsAvailable instanceOfCollectionsAvailable;
	
	public int increment = 0;
	
	// constructors
	public ModelConstants() {
		this.instanceOfExternalMetadata = new ExternalMetadata();
		this.instanceOfListMetadata = new ListMetadata();
		this.instanceOfQualifiersAvailable = new QualifiersAvailable();
		this.instanceOfTagsKeysAvailable = new TagsKeysAvailable();
		this.instanceOfCollectionsAvailable = new CollectionsAvailable();
	}

	// getters
	public ExternalMetadata getInstanceOfExternalMetadata() {
		return this.instanceOfExternalMetadata;
	}
	public ListMetadata getInstanceOfListMetadata() {
		return this.instanceOfListMetadata;
	}
	public QualifiersAvailable getInstanceOfQualifiersAvailable() {
		return this.instanceOfQualifiersAvailable;
	}
	public TagsKeysAvailable getInstanceOfTagsKeysAvailable() {
		return this.instanceOfTagsKeysAvailable;
	}
	public CollectionsAvailable getInstanceOfCollectionsAvailable() {
		return this.instanceOfCollectionsAvailable;
	}
	public int getIncrement() {
		increment += 1;
		return increment - 1;
	}
	
	// functions
	public Map<URI, Reference> getExternalMetadata() {
		return this.instanceOfExternalMetadata.externalMetadata;
	}
	public Map<Index, Metadata> getListMetadata() {
		return this.instanceOfListMetadata.listMetadata;
	}
	public Map<String, Qualifier> getQualifiersAvailable(String component) {
		return this.instanceOfQualifiersAvailable.selectionVariable(component);
	}
	public Set<String> getTagsAvailable() {
		return this.instanceOfTagsKeysAvailable.tags;
	}
	public Map<String, ArrayList<String>> getKeysValuesAvailable() {
		return this.instanceOfTagsKeysAvailable.keysValues;
	}
	public Map<String, Collection> getCollectionsAvailable() {
		return this.instanceOfCollectionsAvailable.getCollections();
	}
	public Map<String, Collection> getOriginalCollections() {
		return this.instanceOfCollectionsAvailable.getOriginalCollections();
	}
}