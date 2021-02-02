package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.grouping.SplittingType;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;

import java.util.Map;
import java.util.List;
import java.util.Random;

/**
 * Random random which wraps a MultipleSuccessorUpdater and picks one of the successors.
 *
 * @author Aurelien Naldi
 */
public class RandomUpdaterWrapper implements RandomUpdater {

    private final MultipleSuccessorsUpdater updater;
    private final Random random = new Random(); 
    
    
    public RandomUpdaterWrapper(MultipleSuccessorsUpdater updater) {
        this.updater = updater;
    }

    @Override
    public byte[] pickSuccessor(byte[] state) {
        List<byte[]> successors = updater.getSuccessors(state);

        if (successors == null || successors.size() == 0) {
            return null; 
        }

        int l = successors.size();
        if (l == 1) {
            return successors.get(0);
        }

        return successors.get( random.nextInt(l) );
    }

    @Override
    public void setSeed(long seed) {
        random.setSeed(seed);
    }

    @Override
    public LogicalModel getModel() {
        return updater.getModel();
    }

    @Override
    public void setFilter(Map<NodeInfo, SplittingType> filter) {
        updater.setFilter(filter);
    }

	public String getUpdaterName() {
		//return "Random " + this.updater.getUpdaterName();
		return "Random uniform";
	}
    
}
