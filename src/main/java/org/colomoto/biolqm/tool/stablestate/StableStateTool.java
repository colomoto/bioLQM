package org.colomoto.biolqm.tool.stablestate;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelTool.class)
public class StableStateTool extends AbstractTool {

	public static final String HELP_LINE = "Search stable states";
	public static final String HELP_MESSAGE = "arguments: asp";

	public StableStateTool() {
		super("stable", HELP_LINE, HELP_MESSAGE, true);
	}

	@Override
	public void run(LogicalModel model, String[] parameters) {
		for (String s: parameters) {
			if ("asp".equalsIgnoreCase(s)) {
				runASP(model);
				return;
			}
		}
		
		runMDD(model);
	}
	
	public void runMDD(LogicalModel model) {
		StableStateSearcher ssearcher = new StableStateSearcher(model);
        try {
            int stable = ssearcher.call();
            MDDManager ddm = ssearcher.getMDDManager();

            PathSearcher psearcher = new PathSearcher(ddm, 1);
            int[] path = psearcher.setNode(stable);
            if (psearcher.countPaths() > 0) {
            	for (NodeInfo node : model.getComponents()) {
            		System.out.print(node.getNodeID() + " ");
            	}
            	System.out.println();
            }
            for (int v: psearcher) {
                for (int i: path) {
                	if (i<0) {
                		System.out.print("-");
                	} else {
                		System.out.print(i);
                	}
                }
                System.out.println();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
	}

	private void runASP(LogicalModel model) {
		StableASP asp = new StableASP(model);
		asp.run();
	}
}
