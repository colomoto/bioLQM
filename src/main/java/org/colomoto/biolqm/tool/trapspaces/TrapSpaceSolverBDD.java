package org.colomoto.biolqm.tool.trapspaces;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.implicants.Formula;
import org.colomoto.biolqm.tool.stablestate.StructuralNodeOrderer;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDFactory;

/**
 * Use BDDs to identify trapspaces.
 * 
 * @author Aurelien Naldi
 */
public class TrapSpaceSolverBDD implements TrapSpaceSolver {

	private final int nvar;
	private final BDDFactory jbdd;
	private final BDD[] constraints;
	private StructuralNodeOrderer order;
	private List<NodeInfo> components;
	private boolean percolate;
	private BDD focus;

	public TrapSpaceSolverBDD(LogicalModel model, TrapSpaceSettings settings) {
		this.percolate = settings.percolate;
		nvar = model.getComponents().size();
		jbdd = BDDFactory.init("java", 50000, 500);
		jbdd.setVarNum(nvar*2);
		focus = jbdd.one();
		constraints = new BDD[nvar];
		order = new StructuralNodeOrderer(model);
		components = model.getComponents();
	}
	
	public void add_focus(int idx) {
		int vidx = 2*idx;
		BDD active = jbdd.ithVar(vidx).andWith(jbdd.nithVar(vidx+1));
		BDD inactive = jbdd.nithVar(vidx).andWith(jbdd.ithVar(vidx+1));
		
		// TODO: refine constraint
		focus.andWith(active);
	}
	
	@Override
	public void add_variable(int idx, Formula formula, Formula not_formula) {
		int[] regulators = formula.regulators;
		BDD f_active = formula2BDD(regulators, formula.toArray());
		BDD f_inactive = formula2BDD(regulators, not_formula.toArray());
		addConstraint(idx, f_active, f_inactive);
	}
	
	@Override
	public void add_fixed(int idx, int value) {
		BDD f_active, f_inactive;
		if (value == 0) {
			f_active = jbdd.zero();
			f_inactive = jbdd.one();
		} else {
			f_active = jbdd.one();
			f_inactive = jbdd.zero();
		}
		addConstraint(idx, f_active, f_inactive);
	}

	private void addConstraint(int idx, BDD f_active, BDD f_inactive) {
		int vidx = 2*idx;
		BDD free = jbdd.nithVar(vidx).andWith(jbdd.nithVar(vidx+1));
		BDD active = jbdd.ithVar(vidx).andWith(jbdd.nithVar(vidx+1));
		BDD inactive = jbdd.nithVar(vidx).andWith(jbdd.ithVar(vidx+1));

		if (percolate) {
			free.andWith(f_inactive.not()).andWith(f_active.not());
		}
		active.andWith(f_active);
		inactive.andWith( f_inactive );
		constraints[idx] = free.orWith(active).orWith(inactive);
		
	}
	
	private BDD formula2BDD(int[] regulators, int[][] terms) {
		BDD cst = jbdd.zero();
		for (int[] term: terms) {
			BDD curcst = jbdd.one();
			for (int i=0 ; i<term.length ; i++) {
				int curidx = 2*regulators[i];
				if (term[i] == 0) {
					curcst.andWith(jbdd.ithVar(curidx+1));
				} else if (term[i] == 1) {
					curcst.andWith(jbdd.ithVar(curidx));
				}
			}
			cst.orWith(curcst);
		}
		return cst;
	}
	
	@Override
	public void solve(TrapSpaceList solutions) {
//		BDD result = jbdd.one();
		BDD result = focus;
		for (int idx: order) {
			BDD next = constraints[idx];
			result.andWith(next);
		}
		List<byte[]> l = result.allsat();
		for (byte[] b: l) {
			byte[] s = new byte[nvar];
			boolean[] variants = new boolean[nvar];
			for (int idx=0 ; idx<nvar ; idx++) {
				int vidx = 2*idx;
				if (b[vidx] > 0) {
					s[idx] = 1;
				} else if (b[vidx+1] > 0) {
					s[idx] = 0;
				} else if (b[vidx] < 0) {
					if (b[vidx+1] < 0) {
						s[idx] = -1;
						variants[idx] = true;
					} else {
						s[idx] = 1;
						variants[idx] = true;
					}
				} else if (b[vidx+1] < 0) {
					s[idx] = 0;
					variants[idx] = true;
				} else {
					s[idx] = -1;
				}
			}
			
			solutions.addPattern(s, variants);
		}
	}

}
