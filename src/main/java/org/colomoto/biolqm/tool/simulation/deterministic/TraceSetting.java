package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.ToolSettings;
import org.colomoto.biolqm.tool.simulation.ordering.DeterministicGrouping;

public class TraceSetting extends ToolSettings {

    DeterministicGrouping grouping = null;
    boolean isSequential = false;
    byte[] state = null;

    public TraceSetting(LogicalModel model) {
        super(model);
    }

}
