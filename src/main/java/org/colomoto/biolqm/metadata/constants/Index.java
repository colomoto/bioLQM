package org.colomoto.biolqm.metadata.constants;

import java.util.ArrayList;

/**
 * Generic class for the indexes, which are used to construct a double-linked list of Metadata objects in ListMetadata
 * It contains the index of the element, the Index of its parent and the Indexes of its children
 *
 * @author Martin Boutroux
 */
public class Index {
	
	// variables
	private int index;
	private Index indexParent;
	private ArrayList<Index> indexChildren;
	
	// constructors	
	public Index(int increment) {
		this.index = increment;
		this.indexParent = null;
		this.indexChildren = new ArrayList<Index>();
	}
	
	public Index(Index parent, int increment) {
		this.index = increment;
		this.indexParent = parent;
		this.indexChildren = new ArrayList<Index>();
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

	// functions
    @Override
	public boolean equals(Object obj) {
        boolean retVal = false;

		Index index = (Index) obj;
        if (index.getIndex() == this.getIndex()) {
            retVal = true;
        }

		return retVal;
	}
	
    @Override
	public int hashCode() {

		return this.getIndex();
	}
}