package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.helper.state.StateList;

public class TrapSpace {

	public final int length;
	public final int nfree, npercolated;
	public final byte[] pattern;
	public final boolean[] percolated;
	
	protected TrapSpace(byte[] pattern) {
		this(pattern, null);
	}
	
	protected TrapSpace(byte[] pattern, boolean[] percolated) {
		this.length = pattern.length;
		this.pattern = pattern;
		if (percolated == null) {
			percolated = new boolean[length];
		}
		this.percolated = percolated;
		
		int nfree = 0;
		int npercolated = 0;
		for (int i=0 ; i<length ; i++) {
			if (pattern[i] < 0) {
				nfree++;
			} else if (percolated[i]) {
				npercolated++;
			}
		}
		this.nfree = nfree;
		this.npercolated = npercolated;
	}
	
	public String toString() {
		String s = "";
    	for (int i=0 ; i<length ; i++) {
    		byte v = pattern[i];
    		switch (v) {
				case StateList.FREE:
					s += "- ";
					break;
				case StateList.JOKER:
					s += "* ";
					break;
				case StateList.UNDEFINED:
					s += "? ";
					break;
				default:
					s += v;
					if (percolated[i]) {
						s += "'";
					} else {
						s += " ";
					}
    		}
    	}
    	return s;
	}
	
	public String shortString() {
		String s = "";
    	for (int i=0 ; i<length ; i++) {
    		byte v = pattern[i];
    		if (v < 0) {
				s += "-";
    		} else {
    			s += v;
    		}
    	}
    	return s;
	}
	
	public boolean contains(TrapSpace t) {
		if (t.nfree >= this.nfree) {
			return false;
		}
		
		for (int i=0 ; i<length ; i++) {
			int v1 = pattern[i];
			int v2 = t.pattern[i];
			if (v1 < 0 || v1 == v2 || percolated[i]) {
				continue;
			}
			return false;
		}
		return true;

	}
	
	@Override
	public int hashCode() {
		return pattern.hashCode();
	}
	
	@Override
	public boolean equals(Object other) {
		if (other instanceof TrapSpace) {
			return pattern.equals(((TrapSpace)other).pattern);
		}
		return false;
	}
}
