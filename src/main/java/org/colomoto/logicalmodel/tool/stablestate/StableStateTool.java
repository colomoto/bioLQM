package org.colomoto.logicalmodel.tool.stablestate;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.tool.AbstractTool;
import org.colomoto.logicalmodel.tool.LogicalModelTool;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelTool.class)
public class StableStateTool extends AbstractTool {

	public StableStateTool() {
		super("stable", "Search stable states", true);
	}

	@Override
	public void run(LogicalModel model) {
		StableStateSearcher ssearcher = new StableStateSearcher(model);
        try {
            int stable = ssearcher.call();
            MDDManager ddm = ssearcher.getMDDManager();

            PathSearcher psearcher = new PathSearcher(ddm, 1);
            int[] path = psearcher.setNode(stable);
            if (psearcher.countPaths() > 0) {
            	for (NodeInfo node : model.getNodeOrder()) {
            		System.out.print(node.getNodeID() + " ");
            	}
            	System.out.println();
            }
            for (int v: psearcher) {
                for (int i: path) {
                    System.out.print(i+" ");
                }
                System.out.println();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
	}

}
