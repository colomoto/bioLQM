package org.colomoto.biolqm.tool.simulation.avatar;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.updater.SequentialUpdater;

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
