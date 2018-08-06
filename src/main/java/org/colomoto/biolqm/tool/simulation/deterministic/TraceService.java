package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.kohsuke.MetaInfServices;

@MetaInfServices(ModelToolService.class)
public class TraceService extends AbstractToolService<DeterministicSimulation, TraceTask> {

    public static final String HELP_LINE = "Compute deterministic trace";
    public static final String HELP_MESSAGE = "arguments: [-u sequential|synchronous] [-i 0010110], [-m #steps]";


    public TraceService() {
        super("trace", HELP_LINE, HELP_MESSAGE, MultivaluedSupport.MULTIVALUED);
    }

    @Override
    public TraceTask getTask(LogicalModel model) {
        return new TraceTask(model);
    }
}
