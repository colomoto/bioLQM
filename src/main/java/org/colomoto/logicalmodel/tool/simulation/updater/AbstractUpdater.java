package org.colomoto.logicalmodel.tool.simulation.updater;

import java.util.Iterator;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.LogicalModelUpdater;

/**
 * Base class for updaters: it remembers the state and provides the iterator.
 * Implementations of specific updating methods just need to implement the code to
 * compute the next state.
 *
 * @author Aurelien Naldi
 */
abstract public class AbstractUpdater implements LogicalModelUpdater {

	protected final LogicalModel model;

	protected byte[] state = null;
	protected byte[] nextState = null;
	protected final int size;
	
	private UpdaterIterator iterator;
	
	protected int status;

	public AbstractUpdater(LogicalModel model) {
		this.model = model;
		this.size = model.getNodeOrder().size();
		this.nextState = null;
		this.iterator = new UpdaterIterator(this);
	}
	
	@Override
	public Iterator<byte[]> iterator() {
		iterator.refresh();
		return iterator;
	}

	@Override
	public void setState(byte[] state) {
		this.state = state;
		reset();
	}

	/**
	 * Test if a component is ready to change its state.
	 * 
	 * @param index the index of the component
	 * @return +1, or -1 for pending increase or decrease, 0 if it can not change state.
	 */
	protected int nodeChange(byte[] state, int index) {
		byte curState = state[index];
		byte nextState = model.getTargetValue(index, state);

		// now see if the node is willing to change it's state
		if (nextState > curState){
		    return 1;
		} else if (nextState < curState){
		    return -1;
		}
		return 0;
	}
	
	/**
	 * Construct the "next" successor for the current state.
	 * Successive calls to this methods allow the iterator to enumerate all successors.
	 * 
	 * Note to implementors: use of the "status" field for state tracking is encouraged.
	 * This field has value "0" when a new state is set, the asynchronous updater uses it
	 * to enumerate components. To use a different state tracker, override the "reset" method.
	 * 
	 * @return the next successor state or null if no more successors is available.
	 */
	public abstract byte[] buildNext();
	
	/**
	 * reset the updater (called when a new state is set).
	 */
	protected void reset() {
		status = 0;
	}
}

/**
 * Iterate on all successor states.
 * 
 * @author Aurelien Naldi
 */
class UpdaterIterator implements Iterator<byte[]> {

	private final AbstractUpdater updater;
	
	private byte[] nextState;

	public UpdaterIterator(AbstractUpdater updater) {
		this.updater = updater;
	}
	
	public void refresh() {
		this.nextState = updater.buildNext();
	}
	
	@Override
	public boolean hasNext() {
		return nextState != null;
	}

	@Override
	public byte[] next() {
		byte[] ret = nextState;
		if (nextState != null) {
			nextState = updater.buildNext();
		}
		return ret;
	}

	@Override
	public void remove() {
		throw new RuntimeException("Removing successor states not supported");
	}
}