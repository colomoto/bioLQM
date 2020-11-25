package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.clingo.ClingoLauncher;
import org.colomoto.biolqm.helper.clingo.ClingoResult;
import org.colomoto.biolqm.helper.clingo.ClingoResultHandler;
import org.colomoto.biolqm.helper.implicants.Formula;
import org.colomoto.biolqm.helper.state.StateList;

import java.io.IOException;
import java.util.*;

/**
 * Use the clingo ASP solver to identify trap spaces.
 * 
 * @author Aurelien Naldi
 */
public class TrapSpaceSolverFunctionASP implements TrapSpaceSolver, ClingoResultHandler {

	private final LogicalModel model;
	private final List<NodeInfo> components;
	private TrapSpaceList solutions;
	TrapSpaceTask settings;

	private final static int[] SWAP = {1,0};

	private final StringBuffer program = new StringBuffer();

	public TrapSpaceSolverFunctionASP(LogicalModel model, TrapSpaceTask settings) {
		this.model = model;
		this.settings = settings;
		this.components = model.getComponents();
		
		program.append("% Define one Boolean variable per possible component value.\n{\n");
		String prefix = "\n  v";
		for (NodeInfo ni: components) {
			String uid = ni.getNodeID();
			program.append(prefix);
			program.append(uid+"0; v"+uid+"1");
			prefix = ";\n  v";
		}
		program.append("\n}.\n");
	}

	@Override
	public void add_variable(int idx, Formula formula, Formula not_formula) {
		String uid = components.get(idx).getNodeID();
		// Consistency rule
		program.append(":- v"+uid+"0, v"+uid+"1.\n");
		add_prime(uid, 1, formula);
		add_prime(uid, 0, not_formula);
		program.append("\n");
	}

	@Override
	public void add_fixed(int idx, int value) {
		if (value < 0 || value > 1) {
			throw new RuntimeException("Invalid value: "+value);
		}
		String uid = components.get(idx).getNodeID();
		program.append("v"+uid+value+ ".\n");
		program.append(":- v"+uid+SWAP[value]+ ".\n");
	}
	
	private void add_prime(String target, int value, Formula formula) {

		// FIXME: generate ASP rules
		for (int[] t: formula.toArray()) {
			program.append(":- v"+target+SWAP[value]);
			for (int i=0 ; i<t.length ; i++) {
				int v = t[i];
				if (v < 0) {
					continue;
				}
				String cur = components.get( formula.regulators[i] ).getNodeID();
				program.append(", not v"+cur+SWAP[v]);
			}
			program.append(".\n");

			if (settings.percolate) {
				program.append("v"+target+value + " :- ");
				String prefix = "";
				for (int i=0 ; i<t.length ; i++) {
					int v = t[i];
					if (v < 0) {
						continue;
					}
					String cur = components.get( formula.regulators[i] ).getNodeID();
					program.append(prefix + "v"+cur+v);
					prefix = ", ";
				}
				program.append(".\n");
			}
		}


	}
	
	public String show() {

		boolean focusMode = false;
		if (settings.focusComponents != null && settings.focusComponents.length > 0) {
			focusMode = true;
			program.append("% TODO: Keep only conditions to fix selected components\n");
			// TODO: implement focus mode
		}

		return program.toString();
	}
	
	@Override
	public void solve(TrapSpaceList solutions) {
		String asp = show();
		this.solutions = solutions;
		ClingoLauncher launcher = new ClingoLauncher(this, asp);
		launcher.setMinsolutions(settings.terminal);
		launcher.setMaxsolutions(settings.generic);
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
		
		Map<String,Integer> hit = new HashMap<String,Integer>();
		List<String[]> lmatches = r.get("");
		if (lmatches != null) {
			for (String[] t: lmatches) {
				// FIXME: proper ID parsing
				if (t.length != 1) {
					continue;
				}
				String match = t[0];
				if (!match.startsWith("v")) {
					continue;
				}
				int l = match.length()-1;
				hit.put(match.substring(1, l), Integer.parseInt(match.substring(l)));
			}
		}

		byte[] pattern = new byte[components.size()];
		for (int idx=0 ; idx<pattern.length ; idx++) {
			String uid = components.get(idx).getNodeID();
			
			if (hit.containsKey(uid)) {
				pattern[idx] = hit.get(uid).byteValue();
			} else {
				pattern[idx] = StateList.FREE;
			}
		}
		
		solutions.add(new TrapSpace(pattern));
	}

	@Override
	public void add_focus(int idx) {
		// TODO Auto-generated method stub
	}
	
}
