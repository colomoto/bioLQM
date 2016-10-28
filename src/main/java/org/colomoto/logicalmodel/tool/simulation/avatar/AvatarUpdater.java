package org.colomoto.logicalmodel.tool.simulation.avatar;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.updater.SequentialUpdater;

/**
 * Wrapper for an updater following a sequential selection scheme
 * @author Rui Henriques
 */
public class AvatarUpdater extends SequentialUpdater {

	/**
	 * Creates an updater with a sequential selection scheme
	 * @param model the logical model
	 */
	public AvatarUpdater(LogicalModel model) {
		super(model);
	}
}
