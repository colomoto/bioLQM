package org.colomoto.logicalmodel.tool.simulation.avatar;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.updater.RandomUpdaterWithRates;

/**
 * Wrapper for an updater following an asynchronous weighted selection scheme
 * @author Rui Henriques
 */
public class MonteCarloUpdater extends RandomUpdaterWithRates {

	/**
	 * Creates an updater with an asynchronous weighted selection scheme
	 * @param model the logical model
	 */
	public MonteCarloUpdater(LogicalModel model) {
		super(model);
	}
}
