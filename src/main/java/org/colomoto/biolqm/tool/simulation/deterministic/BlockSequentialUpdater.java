package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;

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
 * @author Aurelien Naldi
 */
public class BlockSequentialUpdater extends BaseUpdater implements DeterministicUpdater {
	
	private final int[][] blocks;
	
	// cached state to save the current state after updating each block
	private final byte[] tmp_state;

	public BlockSequentialUpdater(ModelGrouping grouping) {
		this(grouping.getModel(), grouping.getDeterministicBlocks());
	}

	public BlockSequentialUpdater(LogicalModel model, int[][] blocks) {
		super(model);
		this.blocks = blocks;
		tmp_state = new byte[model.getComponents().size()];
	}

	public BlockSequentialUpdater(LogicalModel model, String config) {
		this( new ModelGrouping(model, config) );
	}

	@Override
    public byte[] getSuccessor(byte[] state) {
		/*
		 * The state which will be returned, initialized to be current state
		 */
		byte[] nextstate = state.clone();
		
		boolean changed = false;
		
		// Iterate over the blocks (update times) in the list scheme
		for(int[] block: blocks) {
			
			// save the state before this block
			System.arraycopy(nextstate, 0, tmp_state, 0, nextstate.length);
			
			// Iterate over the nodes in the block i
			for(int i=0 ; i<block.length ; i+=2) {
				
				/*
				 * if there is a change in the previous state, we apply it on the next state
				 */
				int k_aux = block[i];
				int cst = block[i+1];
				int change = nodeChange(tmp_state, k_aux);
				if (change != 0 && (cst==0 || cst == change)) {
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
