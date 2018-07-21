package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(ModelToolService.class)
public class TraceTool extends AbstractToolService<DeterministicSimulation, TraceSettings> {

    public static final String HELP_LINE = "Compute deterministic trace";
    public static final String HELP_MESSAGE = "arguments: [-u sequential|synchronous] [-i 0010110], [-m #steps]";


    public TraceTool() {
        super("trace", HELP_LINE, HELP_MESSAGE, true);
    }

    @Override
    public TraceSettings getSettings(LogicalModel model, String... parameters) {
        TraceSettings settings = new TraceSettings(model);
        settings.parseParameters(parameters);
        return settings;
    }

    @Override
    public DeterministicSimulation getResult(TraceSettings settings) throws Exception {
        return settings.getSimulation();
    }

    @Override
    public void run(LogicalModel model, String... parameters) {
        DeterministicSimulation simulation = getSettings(model, parameters).getSimulation();
        byte[] extra = null;
        int n_extra = model.getExtraComponents().size();
        if (n_extra > 0) {
            extra = new byte[n_extra];
        }
        for (byte[] state: simulation) {
            for (byte b: state) {
                System.out.print(b);
            }

            if (extra != null) {
                model.fillExtraValues(state, extra);
                System.out.print("  ");
                for (byte b: extra) {
                    System.out.print(b);
                }
            }
            System.out.println();
        }
    }
}
