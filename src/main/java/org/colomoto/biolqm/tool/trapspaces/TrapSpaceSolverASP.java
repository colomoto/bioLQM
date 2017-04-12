package org.colomoto.biolqm.tool.trapspaces;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.implicants.Formula;

public class TrapSpaceSolverASP implements TrapSpaceSolver {

	private final LogicalModel model;
	private final List<NodeInfo> components;
	private int curprime = 0;
	
	public TrapSpaceSolverASP(LogicalModel model) {
		this.model = model;
		this.components = model.getComponents();
		
		System.out.print("% encoding of prime implicants as hyper-arcs that consist of a unique \"target\" and (possibly) several \"sources\".\n");
		System.out.print("% \"target\" and \"source\" are triplets that consist of a variable name, an activity and a unique arc-identifier.\n");
	}

	public void add_variable(int idx, Formula formula, Formula not_formula) {
		String s_target = components.get(idx).getNodeID();
		add_prime(s_target, 1, formula);
		add_prime(s_target, 0, not_formula);
		
//		System.out.println(idx);
//		System.out.println(formula);
//		System.out.println(not_formula);
//		System.out.println();
	}
	
	private void add_prime(String target, int value, Formula formula) {
		int idx = formula.regulators[0];

		for (int[] t: formula.toArray()) {
			System.out.print("target(\""+target+"\", "+value+", a"+curprime + ").");
			for (int i=0 ; i<t.length ; i++) {
				int v = t[i];
				if (v < 0) {
					continue;
				}
				String cur = components.get( formula.regulators[i] ).getNodeID();
				System.out.print(" source(\""+cur+"\", "+v+", a"+curprime+").");
			}
			System.out.println();
			curprime++;
		}
	}
	
	public void solve() {
		System.out.print("\n\n"
				+ "% generator: \"in_set(ID)\" specifies which arcs are chosen for a trap set (ID is unique for target(_,_,_)).\n"
				+ "{in_set(ID) : target(V,S,ID)}.\n\n"

				+ "% consistency constraint: a node can not be fixed to different values\n"
				+ ":- in_set(ID1), in_set(ID2), target(V,1,ID1), target(V,0,ID2).\n\n"

				+ "% stability constraint: a fixed node must be target in a selected prime\n"
				+ ":- in_set(ID1), source(V,S,ID1), not in_set(ID2) : target(V,S,ID2).\n\n"

				+ "% bijection constraint (bijection between solutions and trap spaces)\n"
				+ "in_set(ID) :- target(V,S,ID); hit(V1,S1) : source(V1,S1,ID); hit(V2,S2) : target(V2,S2,ID).\n\n"

				+ "% \"hit\" captures the stable variables and their activities.\n"
				+ "hit(V,S) :- in_set(ID), target(V,S,ID).\n\n"
				

				+ "% Aurelien: enforce propagation\n"
				+ "in_set(ID) :- target(V,S,ID); hit(V1,S1) : source(V1,S1,ID)."

				
//				+ "% cardinality constraint (enforced by \"Bounds=(1, 1)\")\n"
//				+ ":- {hit(V,S)} 0.\n"
//				+ ":- 25 {hit(V,S)}.\n\n"
				
				+ "% show all (default)\n"
				+ "#show hit/2.\n"
		);
	}
}
