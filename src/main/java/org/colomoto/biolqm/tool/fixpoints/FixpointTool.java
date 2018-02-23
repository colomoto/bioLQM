package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;
import org.mangosdk.spi.ProviderFor;

import java.util.List;


@ProviderFor(LogicalModelTool.class)
public class FixpointTool extends AbstractTool<FixpointList, FixpointSettings> {

	public static final String UID = "fixpoints";
	public static final String[] ALIASES = { "stable", "fixed", "fp" };

	public static final String HELP_LINE = "Search fixed (stable) states";
	public static final String HELP_MESSAGE = "arguments: asp pattern";

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
			if ("pattern".equalsIgnoreCase(p )) {
				settings.pattern = true;
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
		return getMDD(model, false);
	}

	public FixpointList getMDD(LogicalModel model, boolean pattern) {
		FixpointSearcher ssearcher = new FixpointSearcher(model);
		FixpointList result = new FixpointList(model);
		try {
			int stable = ssearcher.call();
			MDDManager ddm = ssearcher.getMDDManager();

			PathSearcher psearcher = new PathSearcher(ddm, 1);
			int[] path = psearcher.setNode(stable);
			if (pattern) {
				for (int v : psearcher) {
					result.add(path.clone());
				}
				ddm.free(stable);
				return result;
			}

			// Find and expand undefined components in the results
			List<NodeInfo> components = model.getComponents();
			for (int v : psearcher) {
				int[] max = path.clone();
				int[] copy = null;
				for (int idx = 0; idx < max.length; idx++) {
					if (max[idx] < 0) {
						if (copy == null) {
							copy = path.clone();
						}
						copy[idx] = 0;
						max[idx] = components.get(idx).getMax();
					}
				}
				if (copy == null) {
					// fully defined state, no need to expand it
					result.add(max);
					continue;
				}

				boolean hasjokers = true;
				result.add(copy.clone());
				while (hasjokers) {
					// We must have at least one unassigned component which can be increased:
					// find it and check if another exists for the next round
					hasjokers = false;
					int idx = max.length - 1;
					for ( ; idx >= 0 ; idx--) {
						// ignore all assigned components
						if (path[idx] >= 0) {
							continue;
						}

						// Reset unassigned components that reached their max value
						// They can be used for the next round
						if (copy[idx] == max[idx]) {
							copy[idx] = 0;
							hasjokers = true;
							continue;
						}

						//
						copy[idx]++;
						break;
					}

					// The next state was found, add it to the list
					result.add(copy.clone());

					if (hasjokers) {
						// we already know that at least one other component can be increased
						continue;
					}

					// Find at least one candidate for the next round
					for ( ; idx >= 0 ; idx--) {
						if (copy[idx] < max[idx]) {
							// found a spot which can change later
							hasjokers = true;
							break;
						}
					}
				}
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
			return getMDD(settings.model, settings.pattern);
		}
	}
}
