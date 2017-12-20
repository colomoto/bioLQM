package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.colomoto.biolqm.tool.simulation.deterministic.TraceSetting;
import org.mangosdk.spi.ProviderFor;

import java.util.List;

/**
 * Entry point for defining simulations in the API
 */
@ProviderFor(LogicalModelTool.class)
public class SimulationService  extends AbstractTool<List<byte[]>, TraceSetting> {

    public static final String HELP_LINE = "Compute deterministic trace";
    public static final String HELP_MESSAGE = "arguments: <updating> <initial>";

    public SimulationService() {
        super("trace", HELP_LINE, HELP_MESSAGE, true);
    }

    @Override
    public TraceSetting getSettings(LogicalModel model, String... parameters) {
        TraceSetting settings = new TraceSetting(model);
        // TODO: parse arguments
        return settings;
    }

    @Override
    public List<byte[]> getResult(TraceSetting settings) throws Exception {
        return null;
    }

    @Override
    public void run(LogicalModel model, String... parameters) {
        System.out.println("TODO: run trace");
    }

}
