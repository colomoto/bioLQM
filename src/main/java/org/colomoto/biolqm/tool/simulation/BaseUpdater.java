package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.grouping.SplittingType;

import java.util.List;
import java.util.Map;

/**
 * Base class for all updaters.
 *
 * @author Aurelien Naldi
 */
abstract public class BaseUpdater implements LogicalModelUpdater {

	protected final LogicalModel model;
	protected final int size;
	protected SplittingType[] filter = null;

	public BaseUpdater(LogicalModel model) {
		this.model = model;
		this.size = model.getComponents().size();
	}

	/**
	 * Test if a component is ready to change its state.
	 *
	 * @param state the base state
	 * @param index the index of the component
	 * @return +1, or -1 for pending increase or decrease, 0 if it can not change
	 *         state.
	 */
	protected int nodeChange(byte[] state, int index) {
		byte curState = state[index];
		SplittingType st = SplittingType.MERGED;
		if (this.filter != null) {
			st = this.filter[index];
			if (st == null) {
				return 0;
			}
		}
		byte nextState = model.getTargetValue(index, state);

		// now see if the node is willing to change it's state
		if (nextState > curState) {
			if (st == SplittingType.NEGATIVE) {
				return 0;
			}
			return 1;
		} else if (nextState < curState) {
			if (st == SplittingType.POSITIVE) {
				return 0;
			}
			return -1;
		}
		return 0;
	}

	public LogicalModel getModel() {
		return model;
	}
	
	/**
	 * Create or update the next state.
	 *
	 * @param state  the base state
	 * @param idx    index of the position to update
	 * @param change change for this position
	 * @param next   a previously created next state (null if it is still unchanged)
	 *
	 * @return an updated next state, cloned from the reference state if needed
	 */
	protected byte[] update(byte[] state, int idx, int change, byte[] next) {
		if (change == 0) {
			return next;
		}

		if (next == null) {
			next = state.clone();
		}
		next[idx] += change;

		return next;
	}

	@Override
	public void setFilter(Map<NodeInfo, SplittingType> filter) {
		if (filter == null || filter.size() == 0) {
			this.filter = null;
		}

		List<NodeInfo> components = model.getComponents();
		this.filter = new SplittingType[components.size()];
		int idx = 0;
		for (NodeInfo ni : components) {
			this.filter[idx++] = filter.get(ni);
		}
	}
	
	@Override
	public abstract UpdaterType getType();
}
