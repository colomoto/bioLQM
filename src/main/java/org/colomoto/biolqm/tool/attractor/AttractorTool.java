package org.colomoto.biolqm.tool.attractor;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.mangosdk.spi.ProviderFor;

/**
 * Identification of synchronous attractors using a SAT solver,
 * as proposed by E. Dubrova and M. Teslenko in 
 * "A SAT-Based Algorithm for Finding Attractors in Synchronous Boolean Networks"
 *
 * @author Mitchell Markin
 */
@ProviderFor(LogicalModelTool.class)
public class AttractorTool extends AbstractTool {

	public AttractorTool() {
		super("attractor", "Find synchronous attractors using SAT", "", false);
	}
	
	@Override
	public void run(LogicalModel model, String ... parameters) {
		SATAttractorFinder finder = new SATAttractorFinder(model);
		finder.run();
	}

	@Override
	public Object getResult(LogicalModel model, Object settings) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}
}
