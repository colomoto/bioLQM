package org.colomoto.biolqm.tool.simulation.random;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;

public class RandomAsynchUpdater extends AbstractRandomUpdater{

	private static String name;

	public RandomAsynchUpdater(LogicalModel model) {
		super(model);
	}

	@Override
	public byte[] pickSuccessor(byte[] state) {
		
		// randomly choose one component
		// update component
		// return successor
		
		// keep track of number of updatable components
		int nb_changes = 0;
		// to keep idx and change that can happen
		int[][] step_changes = new int[this.size][2];
		
		
		// is node ready to change ?
		for (int idx=0 ; idx < size ; idx++) {
			// -1, +1 or 0 (no change)
			int change = nodeChange(state, idx);
			if (change == 0) {
                continue;
            }

            // store the available change
            step_changes[nb_changes][0] = idx;
            step_changes[nb_changes][1] = change;
            nb_changes++;
		}
		
		if (nb_changes == 0) {
            return null;
        }

		byte[] nextstate = state.clone();
		// select between updatable 
        int randomComponent = random.nextInt(nb_changes);
        
        int idx = step_changes[randomComponent][0];
        int change = step_changes[randomComponent][1];
        // System.out.println("Component index chosen: " + idx);

        nextstate[idx] += change;
		return nextstate;
	}

	@Override
	public String getUpdaterName() {
		return name;
	}

}
