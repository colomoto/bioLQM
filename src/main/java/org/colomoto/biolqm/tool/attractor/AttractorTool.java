package org.colomoto.biolqm.tool.attractor;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.colomoto.biolqm.tool.ToolSettings;
import org.mangosdk.spi.ProviderFor;

/**
 * Identification of synchronous attractors using a SAT solver,
 * as proposed by E. Dubrova and M. Teslenko in 
 * "A SAT-Based Algorithm for Finding Attractors in Synchronous Boolean Networks"
 *
 * @author Mitchell Markin
 */
@ProviderFor(ModelToolService.class)
public class AttractorTool extends AbstractToolService {

	public AttractorTool() {
		super("attractor", "Find synchronous attractors using SAT", "", false);
	}
	
	@Override
	public void run(LogicalModel model, String ... parameters) {
		SATAttractorFinder finder = new SATAttractorFinder(model);
		finder.run();
	}

	@Override
	public ToolSettings getSettings(LogicalModel model, String... parameters) {
		return new ToolSettings(model);
	}

	@Override
	public Object getResult(ToolSettings settings) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}
}
