package org.colomoto.logicalmodel.tool.simulation.updater;


import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.MultipleSuccessorsUpdater;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

/**
 * Base class for updaters which yield multiple successors.
 *
 * @author Aurelien Naldi
 */
abstract public class AbstractMultipleSuccessorUpdater extends BaseUpdater implements MultipleSuccessorsUpdater {

    private static final List<byte[]> EMPTYSUCCESSOR = new EmptySuccessorList();

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


    public static List<byte[]> getEmptySuccessors() {
        return EMPTYSUCCESSOR;
    }

    public static List<byte[]> getSingleSuccessors(byte[] next) {
        if (next == null) {
            return EMPTYSUCCESSOR;
        }
        return new SingleStateList(next);
    }

    public static List<byte[]> getSuccessors(byte[][] nexts) {
        if (nexts == null || nexts.length == 0) {
            return EMPTYSUCCESSOR;
        }

        if (nexts.length == 1) {
            return new SingleStateList(nexts[0]);
        }

        return new StateList(nexts);
    }

}



/*
 * simple representation of an empty list of successors
 */
class EmptySuccessorList extends AbstractList<byte[]> {

    @Override
    public byte[] get(int i) {
        return null;
    }

    @Override
    public int size() {
        return 0;
    }
}


class SingleStateList extends AbstractList<byte[]> {

    private final byte[] next;

    public SingleStateList(byte[] state) {
        this.next = state;
    }

    @Override
    public byte[] get(int i) {
        return next;
    }

    @Override
    public int size() {
        return 1;
    }
}

class StateList extends AbstractList<byte[]> {

    private final byte[][] states;

    public StateList(byte[][] states) {
        this.states = states;
    }

    @Override
    public byte[] get(int i) {
        return states[i];
    }

    @Override
    public int size() {
        return states.length;
    }
}
