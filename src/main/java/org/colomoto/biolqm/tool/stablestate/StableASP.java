package org.colomoto.biolqm.tool.stablestate;

import java.io.IOException;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.clingo.ClingoLauncher;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

public class StableASP {

	private final LogicalModel model;
	
	public StableASP(LogicalModel model) {
		this.model = model;
	}
	
	public String getProgram() {
		StableOperation sop = new StableOperation();
		MDDManager ddmanager = model.getMDDManager();
		int[] functions = model.getLogicalFunctions();
		List<NodeInfo> components = model.getComponents();
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
		ClingoLauncher launcher = new ClingoLauncher(program);
		try {
			launcher.run();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
