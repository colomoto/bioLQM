package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;

import java.util.ArrayList;
import java.util.List;

/**
 * Draft for a priority updater: components are grouped in groups, which can
 * be updated synchronously or asynchronously.
 * Only components from the first group with updated components will be taken into account
 *
 * NOTE: this implementation is incomplete:
 * <ul>
 *   <li>it does not handle separation of increasing and decreasing transitions</li>
 *   <li>it does not fully allow to setup the priority classes properly</li>
 * </ul>
 *
 * @author Aurelien Naldi
 */
public class PriorityUpdater extends AbstractMultipleSuccessorUpdater {

	private final List<PriorityClass> priorityClasses;

	public PriorityUpdater(LogicalModel model, int[] o) {
		super(model);
        priorityClasses = generate_scheme(o);
	}

	/**
	 * Converts the integer array provided by the user into a list of priority classes.
	 *
	 * @param o the original array, assigning a block to each component.
	 */
	private static List<PriorityClass> generate_scheme(int[] o) {
		
		List<PriorityClass> priorityClasses = new ArrayList<PriorityClass>();
		if (o.length < 1) {
			return priorityClasses;
		}

		/* 
		 * The following part gets the maximum and minimum time 
		 * of the array o, these are the limits that time points
		 * can take during 1 update sequence; 
		 */ 
		int max = o[0];
		int min = o[0];
		for (int v: o) {
			if (v > max) {
				max = v;
			} else if (v < min) {
				min = v;
			}
		}
		
		// Iterate over the update time values in ascendent order
		for (int t = min ; t <= max ; t++) {
			
			// create the sublist of nodes updated at time t
			List<Integer> s_list = new ArrayList<Integer>();
			
			// The following part gets every appearance of index t in o
			int idx = 0;
			for (int v: o) {
				if (v == t) {
					s_list.add(idx);
				}
				idx++;
			}
			
			// add the selected indices to the list of blocks
			int nb_components = s_list.size();
			if (nb_components > 0) {
				int[] block = new int[nb_components];
				idx = 0;
				for (int v: s_list) {
					block[idx] = v;
					idx++;
				}
                priorityClasses.add(new PriorityClass(block, true, true));
			}
		}

		return priorityClasses;
	}

	@Override
    public List<byte[]> getSuccessors(byte[] state) {
		/*
		 * this the state that will be returned
		 * here it is initialized to be current basal state
		 */
		byte[] nextstate = null;
        List<byte[]> successors = null;
		
		// Iterate over the priority classes
		for(PriorityClass pclass: priorityClasses) {

            // stop if previous block had some changes and this one is not grouped with it
            if (successors != null && pclass.is_new) {
                break;
            }

			// Update the nodes in the current priority class
			for(int idx: pclass.components) {
				
				int change = nodeChange(state, idx);
				if (change != 0) {
                    nextstate = update(state, idx, change, nextstate);
                    if (!pclass.is_synchronous) {
                        successors = addSuccessor(successors, nextstate);
                        nextstate = null;
                    }
				}

			}
            if (nextstate != null) {
                successors = addSuccessor(successors, nextstate);
                nextstate = null;
            }
		}
		
		return successors;
	}

}


class PriorityClass {
    public final int[] components;
    public final boolean is_synchronous;
    public final boolean is_new;

    public PriorityClass(int[] components, boolean is_synchronous, boolean is_new) {
        this.components = components;
        this.is_synchronous = is_synchronous;
        this.is_new = is_new;
    }
}
