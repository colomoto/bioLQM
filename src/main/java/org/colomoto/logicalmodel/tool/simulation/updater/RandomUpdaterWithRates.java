package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Updater picking a random successor in the asynchronous scheme, with optional rates.
 *
 * @author Aurelien Naldi
 */
public class RandomUpdaterWithRates extends AbstractRandomUpdater {

    // the rates associated to each component
    private final double[] rates;

    // the changes and rates which are active for the current step
    private final double[] step_rates;
    private final int[][] step_changes;

	/**
	 * Create a new random updater
	 *
	 * @param model
	 */
    public RandomUpdaterWithRates(LogicalModel model) {
        this(model, null);
    }

    public RandomUpdaterWithRates(LogicalModel model, double[] rates) {
        super(model);
        if (rates == null) {
            this.rates = new double[this.size];
            for (int i=0 ; i<this.rates.length ; i++) {
                this.rates[i] = 1.0;
            }
        } else {
            this.rates = rates;
        }

        // create empty arrays to store the available changes at each steps
        // reusing these arrays means that this class is NOT thread safe
        this.step_rates = new double[this.size];
        this.step_changes = new int[this.size][2];
    }

	@Override
	public byte[] pickSuccessor(byte[] state) {
        int nb_changes = 0;
        double totalrate = 0;

		for (int idx=0 ; idx < size ; idx++) {
			int change = nodeChange(state, idx);
			if (change == 0) {
                continue;
            }

            // store the available change
            double r = this.rates[idx];
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
        int selected = select(nb_changes, totalrate);
        int idx = step_changes[selected][0];
        int change = step_changes[selected][1];
        nextstate[idx] += change;
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
    private int select(int nb_changes, double totalrate) {
        if (nb_changes == 1) {
            return 0;
        }

        int s = 0;
        int idx = 0;
        double r = totalrate * random.nextDouble();
        for (  ; idx<nb_changes ; idx++) {
            s += this.step_rates[idx];
            if (s > r) {
                break;
            }
        }
        return idx;
    }
}
