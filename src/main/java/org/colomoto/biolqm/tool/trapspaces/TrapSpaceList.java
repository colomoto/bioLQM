package org.colomoto.biolqm.tool.trapspaces;

import java.util.ArrayList;


public class TrapSpaceList extends ArrayList<TrapSpace> {

	public final boolean terminal;
	public final boolean tree;
	
	public TrapSpaceList(TrapSpaceSettings settings) {
		this.terminal = settings.terminal;
		this.tree = settings.tree;
	}
	
	public boolean add(TrapSpace t) {
		if (terminal) {
			int n = size();
			int s = n;
			for (int i=0 ; i<n ; i++) {
				TrapSpace o = get(i);
				if (t.contains(o)) {
					return true;
				}
				
				if (o.contains(t)) {
					TrapSpace last = get(n-1);
					set(i, last);
					n--;
					i--;
				}
			}
			
			if (n < s) {
				// triggered the removal of at least one solution
				set(n, t);
				n++;
				if (n < s) {
					removeRange(n, s);
				}
				return true;
			}
			return super.add(t);
		}
		
		
		if (tree) {
			// TODO: build inclusion tree as we go
			return super.add(t);
		}
		
		return super.add(t);
	}
	
	public boolean[][] inclusion() {
		int n = size();
		boolean[][] incl = new boolean[n][n];
		for (int i=0 ; i<n ; i++) {
			TrapSpace t = get(i);
			for (int j=0 ; j<n ; j++) {
				if (i==j) {
					continue;
				}
				incl[i][j] = t.contains(get(j));
			}
		}
		boolean[][] fincl = incl.clone();
		for (int i=0 ; i<n ; i++) {
			for (int p=0 ; p<n ; p++) {
				if (incl[p][i]) {
					for (int j=0 ; j<n ; j++) {
						if (j == i || j == p) {
							continue;
						}
						if (incl[i][j]) {
							incl[p][j] = false;
						}
					}
				}
			}
		}

		return fincl;
	}

}
