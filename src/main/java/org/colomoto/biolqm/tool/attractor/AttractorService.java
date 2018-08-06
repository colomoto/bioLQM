package org.colomoto.biolqm.tool.attractor;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ToolTask;

/**
 * Identification of synchronous attractors using a SAT solver,
 * as proposed by E. Dubrova and M. Teslenko in 
 * "A SAT-Based Algorithm for Finding Attractors in Synchronous Boolean Networks"
 *
 * @author Mitchell Markin
 */
public class AttractorService extends AbstractToolService<Object, SATAttractorFinder> {

	public AttractorService() {
		super("attractor", "Find synchronous attractors using SAT");
	}
	
	@Override
	public SATAttractorFinder getTask(LogicalModel model) {
		return new SATAttractorFinder(model);
	}

}
