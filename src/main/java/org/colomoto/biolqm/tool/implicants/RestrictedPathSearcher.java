package org.colomoto.biolqm.tool.implicants;

import java.util.Iterator;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;


/**
 * Restrict the paths given by a PathSearcher to the list of actual regulators
 * 
 * @author Aurelien Naldi
 */
public class RestrictedPathSearcher implements Iterable<Integer> {

	private final MDDManager ddmanager;
	private final PathSearcher searcher;
	
	private final int[] path;
	
	private boolean[] is_regulator;
	int[] regulators, to_regulator;
	
	byte[] term;
	
	public RestrictedPathSearcher(MDDManager ddmanager, int value) {
		this.ddmanager = ddmanager;
		this.searcher = new PathSearcher(ddmanager, value);
		this.path = searcher.getPath();
	}
	
	/**
	 * Retrieve the list of regulators on which this searcher is restricted.
	 * @return an array of boolean flags: true values correspond to regulators in the list of all variables.
	 */
	public boolean[] getRegulators() {
		return is_regulator;
	}

	public int[] getRegulatorList() {
		return to_regulator;
	}

	public byte[] setNode(int node) {
    	is_regulator = ddmanager.collectDecisionVariables(node);
    	regulators = new int[is_regulator.length];
    	int nbregulators = 0;
    	for (int i=0 ; i<regulators.length ; i++) {
    		if (is_regulator[i]) {
    			regulators[i] = nbregulators++;
    		} else {
    			regulators[i] = -1;
    		}
    	}
    	
    	// build the reversed map
    	to_regulator = new int[nbregulators];
    	int cur = 0;
    	for (int i=0 ; i<regulators.length ; i++) {
    		if (is_regulator[i]) {
    			to_regulator[cur++] = i;
    		}
    	}
    	
		term = new byte[nbregulators];		
		searcher.setNode(node);
		
		return term;
	}
	
	@Override
	public Iterator<Integer> iterator() {
		return new RestrictedIterator();
	}

	class RestrictedIterator implements Iterator<Integer> {

		private final Iterator<Integer> parentIterator;
		
		public RestrictedIterator() {
			parentIterator = searcher.iterator();
		}
		
		@Override
		public boolean hasNext() {
			return parentIterator.hasNext();
		}

		@Override
		public Integer next() {
			Integer ret = parentIterator.next();
			
        	for (int i=0 ; i<term.length ; i++) {
        		int c = path[ to_regulator[i]];
        		if (c<0) {
        			term[i] = Term.DontCare;
        		} else {
        			term[i] = (byte)c;
        		}
        	}

			return ret;
		}
		
	}
}

