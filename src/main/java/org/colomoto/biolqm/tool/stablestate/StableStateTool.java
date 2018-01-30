package org.colomoto.biolqm.tool.stablestate;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;
import org.mangosdk.spi.ProviderFor;


@ProviderFor(LogicalModelTool.class)
public class StableStateTool extends AbstractTool<StableStateList, StableStateSettings> {

	public static final String HELP_LINE = "Search stable states";
	public static final String HELP_MESSAGE = "arguments: asp";

	public StableStateTool() {
		super("stable", HELP_LINE, HELP_MESSAGE, true);
	}

	@Override
	public StableStateSettings getSettings(LogicalModel model, String ... parameters) {
		StableStateSettings settings = new StableStateSettings(model);

		for (String p: parameters) {
			p = p.trim();
			if ("asp".equalsIgnoreCase(p )) {
				settings.method = StableStateMethod.ASP;
			}
		}
		return settings;
	}

	@Override
	public void run(LogicalModel model, String ... parameters) {
		StableStateList result = null;
		try {
			result = getResult(model, parameters);
		} catch(Exception e) {
			System.out.println("Error while constructing the result");
			e.printStackTrace();
			return;
		}
		
		if (result == null || result.size() < 1) {
			System.out.println("NO RESULTS");
			return;
		}
		
		// print out the result
    	for (NodeInfo node : model.getComponents()) {
    		System.out.print(node.getNodeID() + " ");
    	}
    	System.out.println();

    	for (int[] path: result) {
	        for (int i: path) {
	        	if (i<0) {
	        		System.out.print("-");
	        	} else {
	        		System.out.print(i);
	        	}
	        }
	        System.out.println();
    	}
	}

	public StableStateList getMDD(LogicalModel model) {
		StableStateSearcher ssearcher = new StableStateSearcher(model);
		StableStateList result = new StableStateList(model);
		try {
			int stable = ssearcher.call();
			MDDManager ddm = ssearcher.getMDDManager();

			PathSearcher psearcher = new PathSearcher(ddm, 1);
			int[] path = psearcher.setNode(stable);
			for (int v: psearcher) {
				result.add(path.clone());
			}
			ddm.free(stable);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

	private StableStateList getASP(LogicalModel model) {
		StableASP asp = new StableASP(model);
		return asp.get();
	}

	@Override
	public StableStateList getResult(StableStateSettings settings) {
		switch (settings.method) {
		case ASP:
			return getASP(settings.model);
		default:
			return getMDD(settings.model);
		}
	}
}
