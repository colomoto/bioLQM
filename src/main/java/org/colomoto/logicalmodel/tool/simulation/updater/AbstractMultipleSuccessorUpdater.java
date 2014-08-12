package org.colomoto.logicalmodel.tool.simulation.updater;


import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.MultipleSuccessorsUpdater;

/**
 * Base class for updaters which yield multiple successors.
 *
 * @author Aurelien Naldi
 */
abstract public class AbstractMultipleSuccessorUpdater extends BaseUpdater implements MultipleSuccessorsUpdater {

	protected byte[] state = null;
	protected byte[] nextState = null;

	public AbstractMultipleSuccessorUpdater(LogicalModel model) {
        super(model);
		this.nextState = null;
	}
	
}
