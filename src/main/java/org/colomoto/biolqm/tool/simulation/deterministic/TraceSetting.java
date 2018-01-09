package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.ToolSettings;
import org.colomoto.biolqm.tool.simulation.ordering.DeterministicGrouping;

public class TraceSetting extends ToolSettings {

    DeterministicGrouping grouping = null;
    boolean isSequential = false;
    byte[] state = null;
    int max_steps = 1000;

    public TraceSetting(LogicalModel model) {
        super(model);
    }

    public void parseParameters(String ... parameters) {
        for (String s: parameters) {
            if (s.equalsIgnoreCase("sequential")) {
                isSequential = true;
                continue;
            }
            if (s.equalsIgnoreCase("synchronous")) {
                isSequential = false;
                continue;
            }

            if (s.startsWith("max:")) {
                max_steps = Integer.parseInt(s.substring(4));
                continue;
            }

            // parse it as initial state
            int n = s.length();
            if (n == model.getComponents().size()) {
                this.state = new byte[n];
                for (int i=0 ; i<n ; i++) {
                    state[i] = (byte)Character.getNumericValue(s.charAt(i));
                }
            }

        }
    }

    public DeterministicSimulation getSimulation() {
        byte[] state = this.state;
        if (state == null) {
            state = new byte[model.getComponents().size()];
        }

        DeterministicUpdater updater = getUpdater();
        return new DeterministicSimulation(updater, state, max_steps);
    }

    public DeterministicUpdater getUpdater() {
        if (grouping == null) {
        }

        if (isSequential) {
            return new SequentialUpdater(model);
        }
        return new SynchronousUpdater(model);
    }
}
