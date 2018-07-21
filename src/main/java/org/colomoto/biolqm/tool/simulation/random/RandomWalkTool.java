package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(ModelToolService.class)
public class RandomWalkTool extends AbstractToolService<RandomWalkSimulation, RandomWalkSettings> {

    public static final String HELP_LINE = "Perform a random walk in the dynamics";
    public static final String HELP_MESSAGE = "arguments: [-u asynchronous|complete] [-i 0010110], [-m #steps]";


    public RandomWalkTool() {
        super("random", HELP_LINE, HELP_MESSAGE, true);
    }

    @Override
    public RandomWalkSettings getSettings(LogicalModel model, String... parameters) {
        RandomWalkSettings settings = new RandomWalkSettings(model);
        settings.parseParameters(parameters);
        return settings;
    }

    @Override
    public RandomWalkSimulation getResult(RandomWalkSettings settings) throws Exception {
        return settings.getSimulation();
    }

    @Override
    public void run(LogicalModel model, String... parameters) {
        RandomWalkSimulation simulation = getSettings(model, parameters).getSimulation();
        for (byte[] state: simulation) {
            for (byte b: state) {
                System.out.print(b);
            }
            System.out.println();
        }
    }
}
