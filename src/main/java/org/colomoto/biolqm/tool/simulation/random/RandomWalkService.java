package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.kohsuke.MetaInfServices;

@MetaInfServices(ModelToolService.class)
public class RandomWalkService extends AbstractToolService<RandomWalkSimulation, RandomWalkTask> {

    public static final String HELP_LINE = "Perform a random walk in the dynamics";
    public static final String HELP_MESSAGE = "arguments: [-u asynchronous|complete] [-i 0010110], [-m #steps]";


    public RandomWalkService() {
        super("random", HELP_LINE, HELP_MESSAGE, MultivaluedSupport.MULTIVALUED);
    }

    @Override
    public RandomWalkTask getTask(LogicalModel model) {
        RandomWalkTask settings = new RandomWalkTask(model);
        return settings;
    }
}
