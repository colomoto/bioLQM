package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.settings.state.StateList;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.kohsuke.MetaInfServices;


@MetaInfServices(ModelToolService.class)
public class FixpointService extends AbstractToolService<FixpointList, FixpointTask> {

	public static final String UID = "fixpoints";
	public static final String[] ALIASES = { "stable", "fixed", "fp" };

	public static final String HELP_LINE = "Search fixed (stable) states";
	public static final String HELP_MESSAGE = "arguments: asp pattern";

	public FixpointService() {
		super(UID, ALIASES, HELP_LINE, HELP_MESSAGE, MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public FixpointTask getTask(LogicalModel model, String ... parameters) {
		return new FixpointTask(model, parameters);
	}

	@Override
	public void run(LogicalModel model, String ... parameters) {
		StateList result = null;
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

		// print out the result
		NodeInfo[] components = result.getComponents();
		for (NodeInfo ni : components) {
			System.out.print(ni + " ");
		}
    	System.out.println();

		int nrows = result.size();
    	for (int row=0 ; row<nrows ; row++) {
	        for (int col=0 ; col<components.length ; col++) {
	        	int i = result.get(row, col);
	        	if (i == -5) {
	        		System.out.print("?");
				} else if (i<0) {
					System.out.print("-");
				} else {
	        		System.out.print(i);
	        	}
	        }
	        System.out.println();
    	}
	}

}
