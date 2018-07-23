package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(ModelToolService.class)
public class RandomWalkService extends AbstractToolService<RandomWalkSimulation, RandomWalkTask> {

    public static final String HELP_LINE = "Perform a random walk in the dynamics";
    public static final String HELP_MESSAGE = "arguments: [-u asynchronous|complete] [-i 0010110], [-m #steps]";


    public RandomWalkService() {
        super("random", HELP_LINE, HELP_MESSAGE, true);
    }

    @Override
    public RandomWalkTask getTask(LogicalModel model, String... parameters) {
        RandomWalkTask settings = new RandomWalkTask(model);
        settings.parseParameters(parameters);
        return settings;
    }

    @Override
    public void run(LogicalModel model, String... parameters) {
        RandomWalkSimulation simulation = getTask(model, parameters).getSimulation();
        for (byte[] state: simulation) {
            for (byte b: state) {
                System.out.print(b);
            }
            System.out.println();
        }
    }
}
