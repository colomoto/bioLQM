package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;

import java.util.List;

/**
 * Updater for the sequential scheme: all components are updated one after the other in a single successor.
 * Unlike in the synchronous method, the next updated component takes into account the new value of the previous ones.
 * 
 * @author Aurelien Naldi
 */
public class SequentialUpdater extends BaseUpdater implements DeterministicUpdater {

	private final int[] order;
	
	/**
	 * Create a new sequential updater, using the default order
	 * 
	 * @param model the model for which the updater is constructed
	 */
	public SequentialUpdater(LogicalModel model) {
		super(model);
		this.order = new int[size];
		for (int i=0 ; i<size ; i++) {
			order[i] = i;
		}
	}

	/**
	 * Create a new sequential updater, using a custom order
	 *
	 * @param model the model for which the updater is constructed
	 * @param s_order the ordering for sequential updates
	 */
	public SequentialUpdater(LogicalModel model, String s_order) {
		this(model);
		String[] nodes = s_order.split(",");
		int idx = 0;
		List<NodeInfo> components = model.getComponents();
		boolean[] ordered = new boolean[components.size()];
		for (String node: nodes) {
			// Skip mistyped and repeated components
			NodeInfo ni = model.getComponent(node.trim());
			if (ni == null) {
				continue;
			}
			int k = components.indexOf(ni);
			if (ordered[k]) {
				continue;
			}

			// Add this component at the end of the sequential ordering
			ordered[k] = true;
			this.order[idx++] = k;
		}

		// Add unlisted components at the end of the order (following the internal ordering)
		for (int k=0 ; k<size ; k++) {
			if (!ordered[k]) {
				this.order[idx++] = k;
			}
		}
	}

	/**
	 * Create a new sequential updater, using a custom order
	 * 
	 * @param model the model for which the updater is constructed
	 * @param order the ordering for sequential updates
	 */
	public SequentialUpdater(LogicalModel model, int[] order) {
		super(model);
		this.order = order;
	}

	@Override
    public byte[] getSuccessor(byte[] state) {
		// create the sequential successor
        byte[] refstate = state;
		byte[] nextstate = null;
		for (int idx: order) {
            int change = nodeChange(refstate, idx);
            if (change != 0) {
                nextstate = update(refstate, idx, change, nextstate);
                refstate = nextstate;
			}
		}
		
        return nextstate;
	}

	@Override
	public String getUpdaterName() {
		// TODO Auto-generated method stub
		return null;
	}

}
