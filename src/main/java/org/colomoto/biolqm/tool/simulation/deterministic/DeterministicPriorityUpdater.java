package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;

public class DeterministicPriorityUpdater extends BaseUpdater implements DeterministicUpdater {

    int[][] blocks;

    public DeterministicPriorityUpdater(LogicalModel model, int[][] blocks) {
        super(model);
        this.blocks = blocks;
    }

    public DeterministicPriorityUpdater(LogicalModel model, String config) {
        this(new ModelGrouping(model,config));
    }

    public DeterministicPriorityUpdater(ModelGrouping mpc) {
        super(mpc.getModel());
        this.blocks = mpc.getDeterministicBlocks();
    }

    @Override
    public byte[] getSuccessor(byte[] state) {

        byte[] nextstate = null;
        for (int[] block: blocks) {

            // Update the block synchronously
            for (int i=0 ; i<block.length ; i+=2) {
                int idx = block[i];
                int cst = block[i+1];
                int change = nodeChange(state, idx);
                if (change != 0 && (cst == 0 || cst == change)) {
                    nextstate = update(state, idx, change, nextstate);
                }
            }
            if (nextstate != null) {
                break;
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
