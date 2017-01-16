package org.colomoto.biolqm.tool.simulation.updater;

import org.colomoto.biolqm.LogicalModel;

import java.util.ArrayList;
import java.util.List;

/**
 * Updater for the asynchronous scheme: all possible changes are applied separately.
 * It returns all successors according to the order of components in the model.
 * 
 * @author Aurelien Naldi
 */
public class AsynchronousUpdater extends AbstractMultipleSuccessorUpdater {

    private final int[] changes;

	/**
	 * Create a new asynchronous updater.
	 * 
	 * @param model
	 */
	public AsynchronousUpdater(LogicalModel model) {
		super(model);
        this.changes = new int[size];
	}

	@Override
	public List<byte[]> getSuccessors(byte[] state) {

        int nb_changes = 0;
        for (int idx=0 ; idx<size ; idx++) {
            int change = nodeChange(state, idx);
            changes[idx] = change;
            if (change != 0) {
                nb_changes++;
            }
        }

        if (nb_changes == 0) {
            return getEmptySuccessors();
        }

        // fill the list of successors
        List<byte[]> successors = new ArrayList<byte[]>(nb_changes);
        for (int idx=0 ; idx<size ; idx++) {
            int change = changes[idx];
            if (change != 0) {
                successors.add(update(state, idx, change, null));
            }
        }

		return successors;
	}

}
