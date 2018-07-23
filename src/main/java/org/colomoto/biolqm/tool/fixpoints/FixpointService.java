package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.mangosdk.spi.ProviderFor;

import java.util.List;


@ProviderFor(ModelToolService.class)
public class FixpointService extends AbstractToolService<FixpointList, FixpointTask> {

	public static final String UID = "fixpoints";
	public static final String[] ALIASES = { "stable", "fixed", "fp" };

	public static final String HELP_LINE = "Search fixed (stable) states";
	public static final String HELP_MESSAGE = "arguments: asp pattern";

	public FixpointService() {
		super(UID, ALIASES, HELP_LINE, HELP_MESSAGE, true);
	}

	@Override
	public FixpointTask getTask(LogicalModel model, String ... parameters) {
		return new FixpointTask(model, parameters);
	}

	@Override
	public void run(LogicalModel model, String ... parameters) {
		FixpointList result = null;
		try {
			result = getTask(model, parameters).call();
		} catch(Exception e) {
			System.out.println("Error while constructing the result");
			e.printStackTrace();
			return;
		}
		
		if (result == null || result.size() < 1) {
			System.out.println("NO RESULTS");
			return;
		}

		List<byte[]> extra = result.fillExtraNodes();

		// print out the result
		for (NodeInfo node : model.getComponents()) {
			System.out.print(node.getNodeID() + " ");
		}
		if (extra != null) {
			System.out.print("    ");
			for (NodeInfo node : model.getExtraComponents()) {
				System.out.print(node.getNodeID() + " ");
			}
		}
    	System.out.println();

		int idx = 0;
    	for (byte[] path: result) {
	        for (int i: path) {
	        	if (i<0) {
	        		System.out.print("-");
	        	} else {
	        		System.out.print(i);
	        	}
	        }

	        if (extra != null) {
	        	System.out.print("    ");
	        	byte[] extrapath = extra.get(idx++);
				for (int i: extrapath) {
					if (i<0) {
						System.out.print("-");
					} else {
						System.out.print(i);
					}
				}
			}
	        System.out.println();
    	}
	}

}
