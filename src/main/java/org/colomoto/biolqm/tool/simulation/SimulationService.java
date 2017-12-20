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
    public List<byte[]> getResult(LogicalModel model, TraceSetting settings) throws Exception {
        return null;
    }

    @Override
    public void run(LogicalModel model, String... parameters) {

    }

}
