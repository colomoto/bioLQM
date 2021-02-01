package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.LogicalModel;

/**
 * Updater picking a random successor in the asynchronous scheme, with optional rates.
 *
 * @author Aurelien Naldi
 */  
public class RandomUpdaterWithRates extends AbstractRandomUpdater {

    // the rates associated to each component
    private final double[] rates;

	/**
	 * Create a new random random
	 *
	 * @param model the model for which the random is constructed
	 */
    public RandomUpdaterWithRates(LogicalModel model) {
        this(model, null);
    }

    /**
     * 
	 * @param model the model for which the random is constructed
     * @param rates the rates associated to each component
     */
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
    }

	@Override
	public byte[] pickSuccessor(byte[] state) {
        // track the changes and rates which are enabled for the current step
        int nb_changes = 0;
        double totalrate = 0;
        double[] step_rates = new double[size];
        int[][] step_changes = new int[this.size][2];

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
        int selected = select(step_rates, nb_changes, totalrate);
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
    private int select(double[] step_rates, int nb_changes, double totalrate) {
        if (nb_changes == 1) {
            return 0;
        }

        double s = 0;
        int idx = 0;
        double r = totalrate * random.nextDouble();
        for (  ; idx<nb_changes ; idx++) {
            s = step_rates[idx];
            if (s >= r) {
                break;
            }
        }
        return idx;
    }
    
    public double[] getRates() {
    	return this.rates;
    }
}
