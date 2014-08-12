package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Updater picking a random successor in the asynchronous scheme, with optional weight.
 *
 * @author Aurelien Naldi
 */
public class WeightedRandomUpdater extends AbstractRandomUpdater {

    // the weights associated to each component
    private final int[] weights;

    // the changes and weights which are active for the current step
    private final int[] step_weights;
    private final int[][] step_changes;

	/**
	 * Create a new random updater
	 *
	 * @param model
	 */
    public WeightedRandomUpdater(LogicalModel model) {
        this(model, null);
    }

    public WeightedRandomUpdater(LogicalModel model, int[] weights) {
        super(model);
        if (weights == null) {
            this.weights = new int[this.size];
            for (int i=0 ; i<this.weights.length ; i++) {
                this.weights[i] = 1;
            }
        } else {
            this.weights = weights;
        }

        // create empty arrays to store the available changes at each steps
        // reusing these arrays means that this class is NOT thread safe
        this.step_weights = new int[this.size];
        this.step_changes = new int[this.size][2];
    }

	@Override
	public byte[] pickSuccessor(byte[] state) {
        int nb_changes = 0;
        int totalweight = 0;

		for (int idx=0 ; idx < size ; idx++) {
			int change = nodeChange(state, idx);
			if (change == 0) {
                continue;
            }

            // store the available change
            int w = this.weights[idx];
            totalweight += w;
            step_weights[nb_changes] = totalweight;
            step_changes[nb_changes][0] = idx;
            step_changes[nb_changes][1] = change;
            nb_changes++;
		}

        if (nb_changes == 0) {
            return null;
        }

        byte[] nextstate = state.clone();
        int[] selected = select(nb_changes, totalweight);
        nextstate[ selected[0] ] += selected[1];
		return nextstate;
	}

    /**
     * Select one of the weighted changes randomly.
     *
     * It will pick the largest position such that the sum of previous active weights is larger than a random threshold.
     *
     * @param totalweight
     * @return
     */
    private int[] select(int nb_changes, int totalweight) {
        if (nb_changes == 1) {
            return step_changes[0];
        }

        int s = 0;
        int idx = 0;
        int r = random.nextInt(totalweight);
        for (  ; idx<nb_changes ; idx++) {
            s += this.step_weights[idx];
            if (s > r) {
                break;
            }
        }
        return step_changes[idx];
    }
}
