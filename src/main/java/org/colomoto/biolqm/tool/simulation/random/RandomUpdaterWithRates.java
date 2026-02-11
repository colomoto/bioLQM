package org.colomoto.biolqm.tool.simulation.random;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.UpdaterType;
import org.colomoto.biolqm.tool.simulation.grouping.SplittingType;

/**
 * Updater picking a random successor in the asynchronous scheme, with optional
 * rates.
 *
 * @author Aurelien Naldi
 */
public class RandomUpdaterWithRates extends AbstractRandomUpdater {

	// the rates associated to each component
	private final Double[] rates;

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
	public RandomUpdaterWithRates(LogicalModel model, Double[] rates) {
		super(model);
		// if no rates passed
		if (rates == null) {
			List<Double> nodeRates = new ArrayList<Double>();
			List<NodeInfo> nodes = model.getComponents();

			for (NodeInfo node : nodes) {
				nodeRates.add(1.0);
				nodeRates.add(1.0);
			}
			this.rates = new Double[nodeRates.size()];
			for (int i = 0; i < nodeRates.size(); i++)
				this.rates[i] = 1.0;
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

		for (int idx = 0, rates = 0; idx < size; idx++, rates += 2) {
			if (model.getComponents().get(idx).isInput())
				continue;
			int change = nodeChange(state, idx);

			if (change == 0)
				continue;

			double r = 0.0;
			if (this.filter != null && this.filter.length != 0) {
				SplittingType splt = this.filter[idx];
				if (splt.equals(SplittingType.MERGED)) {
					if (change == -1) {
						r = this.rates[rates];
					} else {
						r = this.rates[rates + 1];
					}
				} else if (splt.equals(SplittingType.POSITIVE)) {
					r = this.rates[rates + 1];
				} else if (splt.equals(SplittingType.NEGATIVE)) {
					r = this.rates[rates];
				}
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
	 * It will pick the largest position such that the sum of previous active rates
	 * is larger than a random threshold.
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
		for (; idx <= nb_changes; idx++) {
			s = step_rates[idx];
			if (s >= r) {
				break;
			}
		}
		return idx;
	}

	public Double[] getRates() {
		return this.rates;
	}
	
	public SplittingType getFilterAt(int idx) {
		return this.filter[idx];
	}
	
	@Override
	public UpdaterType getType() {
		return UpdaterType.RAND_NON_UNIF;
	}
}
