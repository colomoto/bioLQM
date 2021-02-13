package org.colomoto.biolqm.tool.simulation.random;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.grouping.SplittingType;

/**
 * Updater picking a random successor in the asynchronous scheme, with optional rates.
 *
 * @author Aurelien Naldi
 */  
public class RandomUpdaterWithRates extends AbstractRandomUpdater {

    // the rates associated to each component
    private final double[] rates;
    private final String name = "Random non uniform";

	/**
	 * Create a new random random
	 *
	 * @param model the model for which the random is constructed
	 */
    public RandomUpdaterWithRates(LogicalModel model) {
        this(model, null, null);
    }
    
    public RandomUpdaterWithRates(LogicalModel model, double[] rates) {
        this(model, rates, null);
    }
    
    public RandomUpdaterWithRates(LogicalModel model,  Map<NodeInfo, SplittingType> filter) {
        this(model, null, filter);
    }

    /**
     * 
	 * @param model the model for which the random is constructed
     * @param rates the rates associated to each component
     */
    public RandomUpdaterWithRates(LogicalModel model, double[] rates, Map<NodeInfo, SplittingType> filter) {
        super(model);
        
        // if no rates passed
        if (rates == null) {
        	if(filter == null) {
        		this.rates = new double[this.size];
        		for (int i=0 ; i< this.rates.length ; i++) {
        			this.rates[i] = 1.0;
        		}
        	} else {
                this.setFilter(filter);
        		// cast from ArrayList to array
        		this.rates = new double[filter.size()*2];
        		Arrays.fill(this.rates, 1.0);
  
        	}
        } else {
        		this.rates = rates;
        		if (filter != null ) 
        			this.setFilter(filter);
        	}
    }

	@Override
	public byte[] pickSuccessor(byte[] state) {
        // track the changes and rates which are enabled for the current step
        int nb_changes = 0;
        double totalrate = 0;
        double[] step_rates = new double[size];
        int[][] step_changes = new int[this.size][2];

        int merged = 0;
		for (int idx=0 ; idx < size ; idx++) {
			int change = nodeChange(state, idx);
						            			
			if (change == 0) {
				merged += 2;
                continue;
            }
			
			double r = 0.0;
	
			if (this.filter != null && this.filter.length != 0) {
				SplittingType splt = this.filter[idx]; 
				if (splt.equals(SplittingType.MERGED)) { 
				
					if (change == -1) {
						r = this.rates[merged];
					} else {
						r = this.rates[merged+1];
					}
				} merged += 2;
			} else {
				r = this.rates[idx];
			}

            // store the available change
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
        for (  ; idx <= nb_changes ; idx++) {
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

	@Override
	public String getUpdaterName() {
		return this.name;
	}
}
