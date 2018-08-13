package org.colomoto.biolqm.tool.fixpoints;

import java.util.List;

import org.colomoto.common.task.AbstractTask;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

/**
 * Perform a stable state search for a given model.
 * This Runnable will take all logical functions in a logical model and combine the using StableOperation.
 * 
 * @author Aurelien Naldi
 */
public class FixpointSearcher extends AbstractTask<Integer> {

	private final LogicalModel model;

	/**
	 * Create a new stable state searcher.
	 * The provided model will be used directly for stable state search:
	 * model modifications (perturbations, reduction, ...) must be applied before.
	 * 
	 * @param model the model for which we search stable states.
	 */
	public FixpointSearcher(LogicalModel model) {
		this.model = model;
	}

    @Override
    protected Integer performTask() {
        Iterable<Integer> ordering = new StructuralNodeOrderer(model);
        StableOperation sop = new StableOperation();
        int[] mdds = model.getLogicalFunctions();

        // loop over the existing nodes!
        int prev=1;
        int result=prev;
        List<NodeInfo> nodes = model.getComponents();
        MDDManager ddmanager = model.getMDDManager();
        for (int i: ordering) {
            NodeInfo node = nodes.get(i);
            prev = result;
            MDDVariable var = ddmanager.getVariableForKey(node);
            int f = mdds[i];
            result = sop.getStable(ddmanager, prev, f, var);
            ddmanager.free(prev);
            if (canceled) {
                ddmanager.free(result);
                return null;
            }
        }

        return result;
    }

	/**
	 * Convenience method to retrieve the MDDManager in which the result is stored.
	 * 
	 * @return the MDDManager
	 */
	public MDDManager getMDDManager() {
		return model.getMDDManager();
	}
	
	/**
	 * Get the path (variable assignment) corresponding to stable states.
	 * This will run the computation if it was not done before, otherwise it will just return a new PathSearcher using the cached result.
	 * 
	 * @return a path iterator for the found stable states
	 */
	public PathSearcher getPaths() {
		int result = getResult();
		PathSearcher ps = new PathSearcher(model.getMDDManager(), 1);
		ps.setNode(result);
		return ps;
	}
}
