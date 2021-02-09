package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.metadata.annotations.URI;

import org.colomoto.biolqm.metadata.constants.ExternalMetadata;
import org.colomoto.biolqm.metadata.constants.ListMetadata;
import org.colomoto.biolqm.metadata.constants.QualifiersAvailable;
import org.colomoto.biolqm.metadata.constants.TagsKeysAvailable;
import org.colomoto.biolqm.metadata.constants.Index;
import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.colomoto.biolqm.metadata.constants.Reference;
import org.colomoto.biolqm.metadata.constants.CollectionsAvailable;
import org.colomoto.biolqm.metadata.constants.Collection;

import java.util.Map;
import java.util.HashMap;
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
}