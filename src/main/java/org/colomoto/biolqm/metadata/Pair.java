package org.colomoto.biolqm.metadata;

/**
 * This contains the source node and the target node of an edge
 *
 * @author Martin Boutroux
 */
public class Pair<T> {
	
	public final T node1;
	public final T node2;
	
	public Pair(T node1, T node2) {
		this.node1 = node1;
		this.node2 = node2;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Pair) {
			Pair<?> nif = (Pair) obj;
			return this.node1.equals(nif.node1) && this.node2.equals(nif.node2);
		}
		return false;
	}
	
    @Override
	public int hashCode() {
    	return this.node1.hashCode() + this.node2.hashCode();
	}
}
