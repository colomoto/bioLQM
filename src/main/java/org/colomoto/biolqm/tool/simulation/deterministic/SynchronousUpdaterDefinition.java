package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;

public class SynchronousUpdaterDefinition implements DeterministicUpdaterDefinition {

    @Override
    public DeterministicUpdater getUpdater(LogicalModel model) {
        return new SynchronousUpdater(model);
    }

}
