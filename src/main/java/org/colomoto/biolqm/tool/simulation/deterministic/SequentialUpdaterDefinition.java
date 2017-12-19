package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;

import java.util.List;

public class SequentialUpdaterDefinition implements DeterministicUpdaterDefinition {

    List<String> order;

    @Override
    public DeterministicUpdater getUpdater(LogicalModel model) {
        return new SequentialUpdater(model);
    }

}
