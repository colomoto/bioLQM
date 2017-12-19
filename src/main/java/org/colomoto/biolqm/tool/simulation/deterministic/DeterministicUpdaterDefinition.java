package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;

public interface DeterministicUpdaterDefinition {

    DeterministicUpdater getUpdater(LogicalModel model);

}
