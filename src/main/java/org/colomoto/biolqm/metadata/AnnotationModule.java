package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.*;

import org.colomoto.biolqm.metadata.constants.*;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;

/**
 * One instance per model containing all the elements relative to the annotation process
 *
 * @author Martin Boutroux
 * @author Aurelien Naldi
 */
public class AnnotationModule {

	/** Keep track of available qualifiers, collections, tags, ... */
	private final ModelConstants modelConstants = new ModelConstants();

	/** Assign metadata to the model itself */
	private Metadata modelMetadata = null;

	/** Assign metadata to each annotated element */
	private final Map<Object, Metadata> annotations = new HashMap<>();

	private LegalAnnotation legal = null;

	/**
	 * Check if any element is annotated in this model
	 * @return true if no annotations exist on the model itself or any of its elements
	 */
	public boolean isEmpty() {
		return this.legal == null && this.modelMetadata == null && this.annotations.isEmpty();
	}

	public LegalAnnotation getLegal() {
		return this.legal;
	}
	public LegalAnnotation ensureLegal() {
		if (this.legal == null) {
			this.legal = new LegalAnnotation();
		}
		return this.legal;
	}

	/**
	 * Retrieve the annotations associated to the current selection.
	 * @return the existing annotation or null if it is not defined.
	 */
	public Metadata getAnnotation(Object selected) {
		if (selected == null) {
			return this.modelMetadata;
		}
		return this.annotations.get(selected);
	}

	/**
	 * Retrieve or create the annotations associated to the current selection.
	 * @return the existing annotation or a new one if it was not defined.
	 */
	public Metadata ensureMetadata(Object selected) {
		if (selected == null) {
			if (this.modelMetadata == null) {
				this.modelMetadata = new Metadata();
			}
			return this.modelMetadata;
		}

		Metadata meta = this.annotations.get(selected);
		if (meta == null) {
			meta = new Metadata();
			this.annotations.put(selected, meta);
		}
		return meta;
	}


	public Qualifier ensureQualifier(AnnotationTarget target, String qualifier) {
		if (qualifier == null || qualifier.trim().isEmpty()) {
			return null;
		}
		return this.modelConstants.getInstanceOfQualifiersAvailable().ensureQualifier(target, qualifier);
	}

	public void useTag(String tag) {
		this.modelConstants.addTag(tag);
	}
	public void useKey(String key) {
		this.modelConstants.addKey(key);
	}

	public Collection getCollection(String col) {
		return this.modelConstants.getInstanceOfCollectionsAvailable().getCollection(col);
	}

	public Iterable<Object> annotated() {
		return this.annotations.keySet();
	}

	public QualifiersAvailable availableQualifiers() {
		return this.modelConstants.getInstanceOfQualifiersAvailable();
	}
	public CollectionsAvailable availableCollections() {
		return this.modelConstants.getInstanceOfCollectionsAvailable();
	}
	public Set<String> availableTags() {
		return this.modelConstants.availableTags();
	}
	public Set<String> availableKeys() {
		return this.modelConstants.availableKeys();
	}
}
