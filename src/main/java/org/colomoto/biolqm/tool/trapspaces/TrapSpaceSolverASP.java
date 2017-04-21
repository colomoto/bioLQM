package org.colomoto.biolqm.tool.trapspaces;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.clingo.ClingoLauncher;
import org.colomoto.biolqm.helper.clingo.ClingoResult;
import org.colomoto.biolqm.helper.clingo.ClingoResultHandler;
import org.colomoto.biolqm.tool.implicants.Formula;

/**
 * Use the clingo ASP solver to identify trap spaces.
 * 
 * @author Aurelien Naldi
 */
public class TrapSpaceSolverASP implements TrapSpaceSolver, ClingoResultHandler {

	private static boolean showPercolations = true;
	
	private final LogicalModel model;
	private final List<NodeInfo> components;
	private int curprime = 0;
	
	private final StringBuffer program = new StringBuffer();
	
	public TrapSpaceSolverASP(LogicalModel model) {
		this.model = model;
		this.components = model.getComponents();
		
		program.append("% encoding of prime implicants as hyper-arcs that consist of a unique \"target\" and (possibly) several \"sources\".\n");
		program.append("% \"target\" and \"source\" are triplets that consist of a variable name, an activity and a unique arc-identifier.\n");
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
			program.append("target(\""+target+"\", "+value+", a"+curprime + ").");
			for (int i=0 ; i<t.length ; i++) {
				int v = t[i];
				if (v < 0) {
					continue;
				}
				String cur = components.get( formula.regulators[i] ).getNodeID();
				program.append(" source(\""+cur+"\", "+v+", a"+curprime+").");
			}
			program.append("\n");
			curprime++;
		}
	}
	
	public void solve() {
		program.append("\n\n"
				+ "% generator: \"in_set(ID)\" specifies which arcs are chosen for a trap set (ID is unique for target(_,_,_)).\n"
				+ "{in_set(ID) : target(V,S,ID)}.\n\n"

				+ "% consistency constraint: a node can not be fixed to different values\n"
				+ ":- in_set(ID1), in_set(ID2), target(V,1,ID1), target(V,0,ID2).\n\n"

				+ "% stability constraint: a fixed node must be target in a selected prime\n"
				+ ":- in_set(ID1), source(V,S,ID1), not in_set(ID2) : target(V,S,ID2).\n\n"

				+ "% \"hit\" captures the stable variables and their activities.\n"
				+ "hit(V,S) :- in_set(ID), target(V,S,ID).\n\n"
				

				+ "% Enforce propagation\n"
				+ "in_set(ID) :- target(V,S,ID); hit(V1,S1) : source(V1,S1,ID).\n\n"

				
				+ "% Detect percolated: nodes which are not part of a selected circuit\n"
				+ "upstream(V1,V2) :- in_set(ID), target(V1,S1,ID), source(V2,S2,ID).\n"
				+ "upstream(V1,V2) :- upstream(V1,V3), upstream(V3,V2).\n"
				+ "percolated(V1) :- hit(V1,S), not upstream(V1,V1).\n\n"
				
//				+ "% cardinality constraint (enforced by \"Bounds=(1, 1)\")\n"
//				+ ":- {hit(V,S)} 0.\n"
//				+ ":- 25 {hit(V,S)}.\n\n"
				
				+ "% show all (default)\n"
				+ "#show hit/2.\n"
				+ "#show percolated/1.\n"
		);

		ClingoLauncher launcher = new ClingoLauncher(this, program.toString());
		try {
			launcher.run();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void handle(ClingoResult r) {
		if (r == null) {
			return;
		}
		
		Set<String> percolated = new HashSet<String>();
		Map<String,Integer> hit = new HashMap<String,Integer>();
		List<String[]> lmatches = r.get("percolated");
		if (lmatches != null) {
			for (String[] t: lmatches) {
				percolated.add(t[0]);
			}
		}
		lmatches = r.get("hit");
		if (lmatches != null) {
			for (String[] t: lmatches) {
				hit.put(t[0], Integer.parseInt(t[1]));
			}
		}
		
		int[] pattern = new int[components.size()];
		boolean[] isPercolated = new boolean[pattern.length];
		for (int idx=0 ; idx<pattern.length ; idx++) {
			String uid = components.get(idx).getNodeID();
			
			if (hit.containsKey(uid)) {
				pattern[idx] = hit.get(uid);
				isPercolated[idx] = percolated != null && percolated.contains(uid);
			} else {
				pattern[idx] = -1;
				isPercolated[idx] = false;
			}
		}
		
		// print the result
		int idx = 0;
		for (int v: pattern) {
			if (v < 0) {
				System.out.print("-");
			} else {
				System.out.print(v);
			}
			if (showPercolations) {
				if (isPercolated[idx]) {
					System.out.print("'");
				} else {
					System.out.print(" ");
				}
			}
			idx++;
		}
		System.out.println();
	}
	
}
