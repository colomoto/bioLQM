package org.colomoto.logicalmodel.tool.simulation.avatar;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.updater.AsynchronousUpdater;

/**
 * Wrapper for an updater following an asynchronous exhaustive selection scheme
 * @author Rui Henriques
 */
public class FirefrontUpdater extends AsynchronousUpdater {

	/**
	 * Creates an updater with an asynchronous exhaustive selection scheme
	 * @param model the logical model
	 */
	public FirefrontUpdater(LogicalModel model) {
		super(model);
	}
}
