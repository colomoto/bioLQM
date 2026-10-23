package org.colomoto.biolqm.tool.simulation.multiplesuccessor;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.UpdaterType;

import java.util.ArrayList;
import java.util.List;

/**
 * Updater for the "complete" mode: include all asynchronous, synchronous and partially synchronous changes.
 * It returns all successors according to the order of components in the model.
 *
 * Warning: this random can generate a huge number of successors and should not be used to large models.
 * It will fail when more than 30 individual changes are available (and may fill the memory before that).
 * 
 * @author Aurelien Naldi
 */
public class CompleteUpdater extends AbstractMultipleSuccessorUpdater {

    private final int[] changes;
    private final int[] changing;

	/**
	 * Create a new full random.
	 * 
	 * @param model the model for which the random is constructed
	 */
	public CompleteUpdater(LogicalModel model) {
		super(model);
		this.changes = new int[size];
		this.changing = new int[size];
	}

	@Override
	public List<byte[]> getSuccessors(byte[] state) {

        int nb_changes = 0;
        for (int idx=0 ; idx<size ; idx++) {
            int change = nodeChange(state, idx);
            changes[idx] = change;
            if (change != 0) {
                changing[nb_changes] = idx;
                nb_changes++;
            }
        }

        if (nb_changes == 0) {
            return getEmptySuccessors();
        }
        if (nb_changes > 30) {
            throw new RuntimeException("Too many successors in complete simulation");
        }
        
        // enumerate successors: each individual changes is a seed for recursive enumeration
        int nb_successors = (int)Math.pow(2, nb_changes) - 1;
        List<byte[]> successors = new ArrayList<byte[]>(nb_successors);
        for (int cur=0 ; cur<nb_changes ; cur++) {
            fillSuccessors(state, cur, nb_changes, successors);
        }

		return successors;
	}

    /*
     * Enumerate and add successors based on the same seed (list of changes)
     */
    private void fillSuccessors(byte[] state, int current, int count, List<byte[]> successors) {
        if (current >= count) {
            return;
        }
        int idx = changing[current];
        int change = changes[idx];
        if (change == 0) {
            return;
        }
        
        byte[] seed = update(state, idx, change, null);
        successors.add(seed);
        
        // recursive calls to enumerate children successors
        for (int c=current+1 ; c<count ; c++) {
            fillSuccessors(seed, c, count, successors);
        }
    }

    @Override
	public UpdaterType getType() {
    	return UpdaterType.COMPLETE;
	}


}

