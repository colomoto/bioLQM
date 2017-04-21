package org.colomoto.biolqm.tool.stablestate;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.clingo.ClingoLauncher;
import org.colomoto.biolqm.helper.clingo.ClingoResult;
import org.colomoto.biolqm.helper.clingo.ClingoResultHandler;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

/**
 * Use the clingo ASP solver to assemble the stability constraints and identify stable states.
 * 
 * @author Aurelien Naldi
 */
public class StableASP implements ClingoResultHandler {

	private final LogicalModel model;
	private final List<NodeInfo> components;
	
	public StableASP(LogicalModel model) {
		this.model = model;
		this.components = model.getComponents();
	}
	
	public String getProgram() {
		StableOperation sop = new StableOperation();
		MDDManager ddmanager = model.getMDDManager();
		int[] functions = model.getLogicalFunctions();
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] stabilities = new int[functions.length];
		for (int i=0 ; i<functions.length ; i++) {
			stabilities[i] = sop.getStable(ddmanager, 1, functions[i], vars[i]);
		}
		
		StringBuffer program = new StringBuffer("{");
		String prefix="v";
		for (MDDVariable v: vars) {
			program.append(prefix+v.key);
			prefix=";v";
		}
		program.append("}.\n\n");
		
		PathSearcher searcher = new PathSearcher(ddmanager, 0);
		for (int i=0 ; i<functions.length ; i++) {
			int[] path = searcher.setNode(stabilities[i]);

			program.append("% Constraints for "+vars[i].key + "\n");
			for (int v: searcher) {
				program.append(":-");
				prefix=" ";
				for (int j=0 ; j<path.length ; j++) {
					int k = path[j];
					if (k<0) {
						continue;
					}
					program.append(prefix);
					prefix = ", ";
					if (k==0) {
						program.append("not ");
					}
					program.append("v"+vars[j].key);
				}
				program.append(".\n");
			}
		}
		return program.toString();
	}
	
	public void run() {
		String program = getProgram();
		ClingoLauncher launcher = new ClingoLauncher(this, program);
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
		
		Set<String> active = new HashSet<String>();
		List<String[]> lmatches = r.get("");
		if (lmatches != null) {
			for (String[] t: lmatches) {
				active.add(t[0].substring(1));
			}
		}
		
		int[] pattern = new int[components.size()];
		for (int idx=0 ; idx<pattern.length ; idx++) {
			String uid = components.get(idx).getNodeID();
			if (active.contains(uid)) {
				pattern[idx] = 1;
			} else {
				pattern[idx] = 0;
			}
		}
		
		// print the result
		for (int v: pattern) {
			System.out.print(v);
		}
		System.out.println();
	}
}

