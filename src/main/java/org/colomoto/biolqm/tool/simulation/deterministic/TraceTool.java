package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.mangosdk.spi.ProviderFor;

import java.util.Iterator;

@ProviderFor(LogicalModelTool.class)
public class TraceTool extends AbstractTool<DeterministicSimulation, TraceSetting> {

    public static final String HELP_LINE = "Compute deterministic trace";
    public static final String HELP_MESSAGE = "arguments: <sequential|synchronous> <initial state>, <max:#steps>";


    public TraceTool() {
        super("trace", HELP_LINE, HELP_MESSAGE, true);
    }

    @Override
    public TraceSetting getSettings(LogicalModel model, String... parameters) {
        TraceSetting settings = new TraceSetting(model);
        settings.parseParameters(parameters);
        return settings;
    }

    @Override
    public DeterministicSimulation getResult(TraceSetting settings) throws Exception {
        return settings.getSimulation();
    }

    @Override
    public void run(LogicalModel model, String... parameters) {
        DeterministicSimulation simulation = getSettings(model, parameters).getSimulation();
        for (byte[] state: simulation) {
            for (byte b: state) {
                System.out.print(b);
            }
            System.out.println();
        }
    }
}
