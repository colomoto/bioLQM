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

	
	private final BDDFactory jbdd;
	private final BDD[] constraints;
	private StructuralNodeOrderer order;
	private List<NodeInfo> components;

	public TrapSpaceSolverBDD(LogicalModel model) {
		int nvar = model.getComponents().size();
		jbdd = BDDFactory.init("java", 50000, 500);
		jbdd.setVarNum(nvar*2);
		constraints = new BDD[nvar];
		order = new StructuralNodeOrderer(model);
		components = model.getComponents();
	}
	
	public void add_variable(int idx, Formula formula, Formula not_formula) {
		
		int[] regulators = formula.regulators;
		int vidx = 2*idx;
		BDD free = jbdd.nithVar(vidx).andWith(jbdd.nithVar(vidx+1));
		BDD active = jbdd.ithVar(vidx).andWith(jbdd.nithVar(vidx+1));
		BDD inactive = jbdd.nithVar(vidx).andWith(jbdd.ithVar(vidx+1));
		BDD f_active = formula2BDD(regulators, formula.toArray());
		BDD f_inactive = formula2BDD(regulators, not_formula.toArray());
		free.andWith(f_inactive.not()).andWith(f_active.not());
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
	
	public void solve() {
		BDD result = jbdd.one();
		for (int idx: order) {
			BDD next = constraints[idx];
			result.andWith(next);
		}
		List<byte[]> l = result.allsat();
		for (byte[] b: l) {
			for (int idx=0 ; idx<components.size() ; idx++) {
				int vidx = 2*idx;
				if (b[vidx] > 0) {
					System.out.print(1);
				} else if (b[vidx+1] > 0) {
					System.out.print(0);
					
				} else if (b[vidx] < 0) {
					if (b[vidx+1] < 0) {
						System.out.print("*");
					} else {
						System.out.print("+");
					}
				} else if (b[vidx+1] < 0) {
 					System.out.print("/");
				} else {
 					System.out.print("-");
				}
			}
			System.out.println();
		}
	}

}
