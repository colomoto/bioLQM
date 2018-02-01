package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;
import org.mangosdk.spi.ProviderFor;


@ProviderFor(LogicalModelTool.class)
public class FixpointTool extends AbstractTool<FixpointList, FixpointSettings> {

	public static final String UID = "fixpoints";
	public static final String[] ALIASES = { "stable", "fixed", "fp" };

	public static final String HELP_LINE = "Search fixed (stable) states";
	public static final String HELP_MESSAGE = "arguments: asp";

	public FixpointTool() {
		super(UID, ALIASES, HELP_LINE, HELP_MESSAGE, true);
	}

	@Override
	public FixpointSettings getSettings(LogicalModel model, String ... parameters) {
		FixpointSettings settings = new FixpointSettings(model);

		for (String p: parameters) {
			p = p.trim();
			if ("asp".equalsIgnoreCase(p )) {
				settings.method = FixpointMethod.ASP;
			}
		}
		return settings;
	}

	@Override
	public void run(LogicalModel model, String ... parameters) {
		FixpointList result = null;
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

	public FixpointList getMDD(LogicalModel model) {
		FixpointSearcher ssearcher = new FixpointSearcher(model);
		FixpointList result = new FixpointList(model);
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

	private FixpointList getASP(LogicalModel model) {
		StableASP asp = new StableASP(model);
		return asp.get();
	}

	@Override
	public FixpointList getResult(FixpointSettings settings) {
		switch (settings.method) {
		case ASP:
			return getASP(settings.model);
		default:
			return getMDD(settings.model);
		}
	}
}
