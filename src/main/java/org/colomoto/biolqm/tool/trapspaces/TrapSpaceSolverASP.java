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

	private final LogicalModel model;
	private final List<NodeInfo> components;
	private int curprime = 0;
	private TrapSpaceList solutions;
	TrapSpaceSettings settings;
	
	private final StringBuffer program = new StringBuffer();
	
	public TrapSpaceSolverASP(LogicalModel model, TrapSpaceSettings settings) {
		this.model = model;
		this.settings = settings;
		this.components = model.getComponents();
		
		program.append("% encoding of prime implicants as hyper-arcs that consist of a unique \"target\" and (possibly) several \"sources\".\n");
		program.append("% \"target\" and \"source\" are triplets that consist of a variable name, an activity and a unique arc-identifier.\n");
	}

	@Override
	public void add_variable(int idx, Formula formula, Formula not_formula) {
		String s_target = components.get(idx).getNodeID();
		add_prime(s_target, 1, formula);
		add_prime(s_target, 0, not_formula);
	}

	@Override
	public void add_fixed(int idx, int value) {
		String s_target = components.get(idx).getNodeID();
		program.append("target(\""+s_target+"\", "+value+", a"+curprime + ").\n");
		curprime++;
	}
	
	private void add_prime(String target, int value, Formula formula) {

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
	
	public String getASP() {
		program.append("\n\n"
				+ "% generator: \"in_set(ID)\" specifies which arcs are chosen for a trap set (ID is unique for target(_,_,_)).\n"
				+ "{in_set(ID) : target(V,S,ID)}.\n\n"

				+ "% consistency constraint: a node can not be fixed to different values\n"
				+ ":- in_set(ID1), in_set(ID2), target(V,1,ID1), target(V,0,ID2).\n\n"

				+ "% stability constraint: a fixed node must be target in a selected prime\n"
				+ ":- in_set(ID1), source(V,S,ID1), not in_set(ID2) : target(V,S,ID2).\n\n"

				+ "% \"hit\" captures the stable variables and their activities.\n"
				+ "hit(V,S) :- in_set(ID), target(V,S,ID).\n\n"
				);

		if (settings.percolate) {
			program.append(
					"% Enforce propagation\n"
					+ "in_set(ID) :- target(V,S,ID); hit(V1,S1) : source(V1,S1,ID).\n\n"
	
					+ "% Detect percolated: nodes which are not part of a selected circuit\n"
					+ "upstream(V1,V2) :- in_set(ID), target(V1,S1,ID), source(V2,S2,ID).\n"
					+ "upstream(V1,V2) :- upstream(V1,V3), upstream(V3,V2).\n"
					+ "percolated(V1) :- hit(V1,S), not upstream(V1,V1).\n\n"
					
					
					);
		} else {
			program.append(
					"% bijection constraint (bijection between solutions and trap spaces), avoids duplicate results\n"
				  + "in_set(ID) :- target(V,S,ID), hit(V,S), hit(V1,S1) : source(V1,S1,ID).\n\n"
			);
		}
		
//		+ "% cardinality constraint (enforced by \"Bounds=(1, 1)\")\n"
//		+ ":- {hit(V,S)} 0.\n"
//		+ ":- 25 {hit(V,S)}.\n\n"

		program.append(
				"% show all (default)\n"
				+ "#show hit/2.\n"
				);

		if (settings.percolate) {
			program.append("#show percolated/1.\n");
		}
		
		return program.toString();
	}
	
	@Override
	public void solve(TrapSpaceList solutions) {
		String asp = getASP();
		this.solutions = solutions;
		ClingoLauncher launcher = new ClingoLauncher(this, asp);
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
		
		byte[] pattern = new byte[components.size()];
		boolean[] isPercolated = new boolean[pattern.length];
		for (int idx=0 ; idx<pattern.length ; idx++) {
			String uid = components.get(idx).getNodeID();
			
			if (hit.containsKey(uid)) {
				pattern[idx] = hit.get(uid).byteValue();
				if (percolated != null && percolated.contains(uid)) {
					isPercolated[idx] = true;
				} else {
					isPercolated[idx] = false;
				}
			} else {
				pattern[idx] = -1;
				isPercolated[idx] = false;
			}
		}
		
		solutions.add(new TrapSpace(pattern, isPercolated));
	}
	
}
