package org.colomoto.logicalmodel.tool.simulation.updater;


import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.MultipleSuccessorsUpdater;

import java.util.ArrayList;
import java.util.List;

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

    public List<byte[]> addSuccessor(List<byte[]> successors, byte[] state) {
        if (state == null) {
            return successors;
        }

        if (successors == null) {
            successors = new ArrayList<byte[]>();
        }
        successors.add(state);

        return successors;
    }
}
