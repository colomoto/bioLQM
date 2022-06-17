package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.Metadata;

import java.util.ArrayList;

/**
 * Generic class for the indexes, which are used to construct a double-linked list of Metadata objects in ListMetadata
 * It contains the index of the element, the Index of its parent and the Indexes of its children
 *
 * @author Martin Boutroux
 */
public class Index {

	// variables
	private final int index;
	private final Index indexParent;
	private final ArrayList<Index> indexChildren;
	
	// constructors	

	public Index(Index parent, int increment) {
		this.index = increment;
		this.indexParent = parent;
		this.indexChildren = new ArrayList<>();
	}
	
	// getters
	public int getIndex() {
		return this.index;
	}
	public Index getIndexOfParent() {
		return this.indexParent;
	}
	public ArrayList<Index> getIndexOfChildren() {
		return this.indexChildren;
	}
	
	// setters
	public void setIndexOfChildren(Index child) {
		if (!this.indexChildren.contains(child)) {
			this.indexChildren.add(child);
		}
		else {
			this.indexChildren.remove(child);
		}
	}

    @Override
	public boolean equals(Object obj) {
		if (obj instanceof Index) {
			Index index = (Index) obj;
			return index.getIndex() == this.getIndex();
		}
		return false;
	}
	
    @Override
	public int hashCode() {
		return this.getIndex();
	}
}