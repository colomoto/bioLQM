package org.colomoto.biolqm.tool.trapspaces;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;


public class TrapSpaceList extends ArrayList<TrapSpace> {

	public final boolean terminal;
	public final List<NodeInfo> nodes;
	
	public TrapSpaceList(TrapSpaceSettings settings, LogicalModel model) {
		this.terminal = settings.terminal;
		this.nodes = model.getComponents();
	}

	public boolean addPattern(byte[] pattern, boolean[] variant) {
		if (terminal) {
			if (variant != null) {
				int nvariant = 0;
				for (int i=0 ; i<variant.length ; i++) {
					if (pattern[i] < 0 && variant[i]) {
						nvariant++;
					}
				}
				if (nvariant > 0) {
					// This pattern contains several terminals
					return unfoldTerminal(pattern, variant, nvariant);
				}
			}
			return add( new TrapSpace(pattern));
		}
		
		if (variant != null) {
			int nvariants = 0;
			for (boolean b: variant) {
				if (b) {
					nvariants++;
				}
			}
			if (nvariants > 0) {
				return unfold(pattern, variant, nvariants);
			}
		}
		return add( new TrapSpace(pattern));
	}
	
	private boolean unfoldTerminal(byte[] pattern, boolean[] variant, int nvariants) {
		byte[] cur = pattern.clone();
		int[] jokers = new int[nvariants];
		int k = 0;
		for (int i=0 ; i<variant.length ; i++) {
			if (pattern[i] < 0 && variant[i]) {
				jokers[k++] = i;
				cur[i] = 0;
			}
		}

		add( new TrapSpace(cur.clone()));

		int curj = 0;
		while (curj < nvariants) {
			int pos = jokers[curj];
			byte curv = cur[pos];
			boolean changed = false;
			if (curv != 1) {
				cur[pos] = 1;
				changed = true;
			}

			if (changed) {
				for (int j=0 ; j<curj ; j++) {
					cur[jokers[j]] = 0;
				}
				add( new TrapSpace(cur.clone()));
				curj = 0;
			} else {
				curj++;
			}
		}
		return true;
	}
	
	
	private boolean unfold(byte[] pattern, boolean[] variant, int nvariants) {
		byte[] cur = pattern.clone();
		int[] jokers = new int[nvariants];
		int k = 0;
		for (int i=0 ; i<variant.length ; i++) {
			if (variant[i]) {
				jokers[k++] = i;
				cur[i] = -1;
			}
		}

		add( new TrapSpace(cur.clone()));

		int curj = 0;
		while (curj < nvariants) {
			int pos = jokers[curj];
			byte curv = cur[pos];
			byte target = pattern[pos];
			boolean changed = false;
			if (target < 0) {
				if (curv == -1 ) {
					cur[pos] = 0;
					changed = true;
				} else if (curv == 0) {
					cur[pos] = 1;
					changed = true;
				}
			} else if (curv != target) {
				cur[pos] = target;
				changed = true;
			}

			if (changed) {
				for (int j=0 ; j<curj ; j++) {
					cur[jokers[j]] = -1;
				}
				add( new TrapSpace(cur.clone()));
				curj = 0;
			} else {
				curj++;
			}
		}
		return true;
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
		
		return super.add(t);
	}
	
	public List<Integer>[] getInclusiontree() {
		
		int n = size();
		List<Integer> roots = new ArrayList<Integer>();
		List<Integer>[] inclusions = new List[n];
		for (int i=0 ; i<n ; i++) {
			TrapSpace t = get(i);
			place(t,i, roots, inclusions);
		}
		return inclusions;
	}

	/**
	 * The current trapspace is not included in the parent, but may include one of the children.
	 * TODO: detect incompatibilities to avoid visiting all branches
	 */
	private void lookup(TrapSpace t, int idx, List<Integer> roots, List<Integer>[] inclusions) {
		List<Integer> children = inclusions[idx];
		for (Integer i: roots) {
			TrapSpace o = get(i);
			if (t.contains(o)) {
				if (children == null) {
					children = new ArrayList<Integer>();
					inclusions[idx] = children;
				}
				if (!children.contains(i)) {
					children.add(i);
				}
			} else {
				List<Integer> nexts = inclusions[i];
				if (nexts != null) {
					lookup(t, idx, nexts, inclusions);
				}
			}
		}
	}

	private void place(TrapSpace t, int idx, List<Integer> roots, List<Integer>[] inclusions) {
		if (roots.contains(idx)) {
			return;
		}
		
		boolean included = false;
		for (Integer i: roots) {
			TrapSpace o = get(i);
			List<Integer> nexts = inclusions[i];
			if (o.contains(t)) {
				included = true;
				if (nexts == null) {
					nexts = new ArrayList<Integer>();
					inclusions[i] = nexts;
				}
				place(t, idx, nexts, inclusions);
			} else if (nexts != null) {
				lookup(t, idx, nexts, inclusions);
			}
		}
		
		if (included) {
			return;
		}
		
		int n = roots.size();
		int s = n;
		for (int ii=0 ; ii<n ; ii++) {
			Integer i = roots.get(ii);
			TrapSpace o = get(i);
			List<Integer> nexts = inclusions[idx];
			if (t.contains(o)) {
				if (nexts == null) {
					nexts = new ArrayList<Integer>();
					inclusions[idx] = nexts;
				}
				place(o, i, nexts, inclusions);
				n--;
				roots.set(ii, roots.get(n));
				ii--;
			} else if (nexts != null) {
				lookup(t, idx, nexts, inclusions);
			}
		}
		
		if (s < n) {
			// triggered the removal of at least one solution
			roots.set(n, idx);
			n++;
			if (n < s) {
				removeRange(n, s);
			}
			return;
		}
		
		if (!roots.contains(idx)) {
			roots.add(idx);
		}
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

	public int getNVars() {
		if (size() == 0) {
			return 0;
		}
		return get(0).length;
	}

}
