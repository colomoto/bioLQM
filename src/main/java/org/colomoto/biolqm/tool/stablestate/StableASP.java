package org.colomoto.biolqm.tool.stablestate;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

public class StableASP {

	private final LogicalModel model;
	
	public StableASP(LogicalModel model) {
		this.model = model;
	}
	
	public void run() {
		StableOperation sop = new StableOperation();
		MDDManager ddmanager = model.getMDDManager();
		int[] functions = model.getLogicalFunctions();
		List<NodeInfo> components = model.getComponents();
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] stabilities = new int[functions.length];
		for (int i=0 ; i<functions.length ; i++) {
			stabilities[i] = sop.getStable(ddmanager, 1, functions[i], vars[i]);
		}
		
		System.out.print("{");
		String prefix="v";
		for (MDDVariable v: vars) {
			System.out.print(prefix+v.key);
			prefix=";v";
		}
		System.out.println("}.\n");
		
		PathSearcher searcher = new PathSearcher(ddmanager, 0);
		for (int i=0 ; i<functions.length ; i++) {
			int[] path = searcher.setNode(stabilities[i]);

			System.out.println("% Constraints for "+vars[i].key);
			for (int v: searcher) {
				System.out.print(":-");
				prefix=" ";
				for (int j=0 ; j<path.length ; j++) {
					int k = path[j];
					if (k<0) {
						continue;
					}
					System.out.print(prefix);
					prefix = ", ";
					if (k==0) {
						System.out.print("not ");
					}
					System.out.print("v"+vars[j].key);
				}
				System.out.println(".");
			}
		}
	}
}
