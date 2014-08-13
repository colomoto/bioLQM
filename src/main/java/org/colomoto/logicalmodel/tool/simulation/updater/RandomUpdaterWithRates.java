package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Updater picking a random successor in the asynchronous scheme, with optional rates.
 *
 * @author Aurelien Naldi
 */
public class RandomUpdaterWithRates extends AbstractRandomUpdater {

    // the rates associated to each component
    private final int[] rates;

    // the changes and rates which are active for the current step
    private final int[] step_rates;
    private final int[][] step_changes;

	/**
	 * Create a new random updater
	 *
	 * @param model
	 */
    public RandomUpdaterWithRates(LogicalModel model) {
        this(model, null);
    }

    public RandomUpdaterWithRates(LogicalModel model, int[] rates) {
        super(model);
        if (rates == null) {
            this.rates = new int[this.size];
            for (int i=0 ; i<this.rates.length ; i++) {
                this.rates[i] = 1;
            }
        } else {
            this.rates = rates;
        }

        // create empty arrays to store the available changes at each steps
        // reusing these arrays means that this class is NOT thread safe
        this.step_rates = new int[this.size];
        this.step_changes = new int[this.size][2];
    }

	@Override
	public byte[] pickSuccessor(byte[] state) {
        int nb_changes = 0;
        int totalrate = 0;

		for (int idx=0 ; idx < size ; idx++) {
			int change = nodeChange(state, idx);
			if (change == 0) {
                continue;
            }

            // store the available change
            int r = this.rates[idx];
            totalrate += r;
            step_rates[nb_changes] = totalrate;
            step_changes[nb_changes][0] = idx;
            step_changes[nb_changes][1] = change;
            nb_changes++;
		}

        if (nb_changes == 0) {
            return null;
        }

        byte[] nextstate = state.clone();
        int[] selected = select(nb_changes, totalrate);
        nextstate[ selected[0] ] += selected[1];
		return nextstate;
	}

    /**
     * Select one of the rated changes randomly.
     *
     * It will pick the largest position such that the sum of previous active rates is larger than a random threshold.
     *
     * @param totalrate
     * @return
     */
    private int[] select(int nb_changes, int totalrate) {
        if (nb_changes == 1) {
            return step_changes[0];
        }

        int s = 0;
        int idx = 0;
        int r = random.nextInt(totalrate);
        for (  ; idx<nb_changes ; idx++) {
            s += this.step_rates[idx];
            if (s > r) {
                break;
            }
        }
        return step_changes[idx];
    }
}
