package org.colomoto.biolqm.tool.implicants;

import java.util.Iterator;

import org.colomoto.mddlib.MDDManager;


/**
 * Restrict the paths given by a PathSearcher to the list of actual regulators
 * 
 * @author Aurelien Naldi
 */
public class ImplicantSearcher implements Iterable<Integer> {

	private final MDDManager ddmanager;
	private final RestrictedPathSearcher searcher;
	
	byte[] pattern, term;
	
	public ImplicantSearcher(MDDManager ddmanager, int value) {
		this.ddmanager = ddmanager;
		this.searcher = new RestrictedPathSearcher(ddmanager, value);
	}
	
	public byte[] setNode(int node) {
		pattern = searcher.setNode(node);
		term = new byte[pattern.length];
		
		return term;
	}
	
	public boolean[] getRegulators() {
		return searcher.getRegulators();
	}
	public int[] getRegulatorList() {
		return searcher.getRegulatorList();
	}
	
	@Override
	public Iterator<Integer> iterator() {
		return new ImplicantIterator();
	}

	class ImplicantIterator implements Iterator<Integer> {

		private final Iterator<Integer> parentIterator = searcher.iterator();
		private Integer returnedValue;

		boolean[] jokers = new boolean[pattern.length];
		
		private int nextInPattern = -1;

		
		@Override
		public boolean hasNext() {
			if (nextInPattern > -1) {
				return true;
			}
			return parentIterator.hasNext();
		}

		@Override
		public Integer next() {
			if (nextInPattern < 0) {
				// retrieve a new pattern
				returnedValue = parentIterator.next();
				for (int i=0 ; i< pattern.length ; i++) {
					if (pattern[i] == Term.DontCare) {
						term[i] = 0;
						jokers[i] = true;
					} else {
						term[i] = pattern[i];
						jokers[i] = false;
					}
				}
			} else {
				// Advance in the current pattern
				term[nextInPattern] = 1;
				for (int j=0 ; j<nextInPattern ; j++) {
					if (jokers[j]) {
						term[j] = 0;
					}
				}
			}
			
			// Check if the current pattern contains more terms
			for ( nextInPattern=0 ; nextInPattern<pattern.length ; nextInPattern++) {
				if (jokers[nextInPattern] && term[nextInPattern] == 0) {
					return returnedValue;
				}
			}
			
			nextInPattern = -1;
			return returnedValue;
		}
	}
}

