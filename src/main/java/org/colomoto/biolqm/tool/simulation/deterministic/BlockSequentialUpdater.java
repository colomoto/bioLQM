package org.colomoto.biolqm.tool.simulation.deterministic;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;

/**
 * Updater for a block-sequential scheme: there are groups of nodes updated in 
 * synchronous way, but the different groups are updated in sequential way, one after
 * another.
 * 
 * The semantics of the Integer array provided by the user is the following: 
 * The length of the array is equal to the number of nodes in the model
 * 
 * order[i]==j iff the node i is updated in the time j
 * 
 * many nodes could be updated in time j, but each node is updated
 * exactly once in one update sequence given by the block-sequential scheme
 * 
 * the nodes are updated in ascending order: if MIN corresponds to the minimum
 * value of the array, the code first update (synchronously) the nodes with 
 * time MIN, then the nodes with time MIN+1, and so on 
 * 
 * @author Francisco Plana
 */
public class BlockSequentialUpdater extends BaseUpdater implements DeterministicUpdater {
	
	private final List<int[]> blocks;
	
	// cached state to save the current state after updating each block
	private final byte[] tmp_state;

	public BlockSequentialUpdater(LogicalModel model, int[] o) {
		super(model);
		blocks = generate_scheme(o);
		tmp_state = new byte[o.length];
	}

	/**
	 * Converts the integer array provided by the user into a list of
	 * integer arrays. Each subarray stores the IDs of nodes in a separate block,
	 * which will be updated synchronously.
	 * these arrays are ordered in an ascendent way according to the updating time.
	 *
	 * @param o the original array, assigning a block to each component.
	 */
	private static List<int[]> generate_scheme(int[] o) {
		
		List<int[]> blocks = new ArrayList<int[]>();
		if (o.length < 1) {
			return blocks;
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
				blocks.add(block);
			}
		}

		return blocks;
	}

	@Override
    public byte[] getSuccessor(byte[] state) {
		/*
		 * this the state that will be returned
		 * here it is initialized to be current basal state
		 */
		byte[] nextstate = state.clone();
		
		boolean changed = false;
		
		// Iterate over the blocks (update times) in the list scheme
		for(int[] block: blocks) {
			
			// save the state before this block
			System.arraycopy(nextstate, 0, tmp_state, 0, nextstate.length);
			
			// Iterate over the nodes in the block i
			for(int k_aux: block) {
				
				/*
				 * if there is a change in the previous state, we apply it on the 
				 * next state
				 */
				int change = nodeChange(tmp_state, k_aux);
				if (change != 0) {
					nextstate[k_aux] += change;
					changed = true;
				}
			}
		}
		
		if (changed) {
			return nextstate;
		}
		return null;
		
	}

}
