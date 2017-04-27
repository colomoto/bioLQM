package org.colomoto.biolqm.tool.trapspaces;

public class TrapSpace {

	public final int length;
	public final int nfree, npercolated;
	public final byte[] pattern;
	public final boolean[] percolated;
	public final boolean[] variants;
	
	public TrapSpace(byte[] pattern) {
		this(pattern, null, null);
	}
	
	public TrapSpace(byte[] pattern, boolean[] percolated, boolean[] variants) {
		this.length = pattern.length;
		this.pattern = pattern;
		if (percolated == null) {
			this.percolated = new boolean[length];
		} else {
			this.percolated = percolated;
		}
		if (variants == null) {
			this.variants = new boolean[length];
		} else {
			this.variants = variants;
		}
		int nfree = 0;
		int npercolated = 0;
		for (int i=0 ; i<length ; i++) {
			if (pattern[i] < 0) {
				nfree++;
			} else if (percolated[i]) {
				
			}
		}
		this.nfree = nfree;
		this.npercolated = npercolated;
	}
	
	public String toString() {
		String s = "";
    	for (int i=0 ; i<length ; i++) {
    		byte v = pattern[i];
    		if (v < 0) {
    			s += "- ";
    		} else {
    			s += v;
    			if (percolated[i]) {
    				s += "'";
    			} else if (variants[i]) {
    				s += "~";
    			} else {
    				s += " ";
    			}
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
}
