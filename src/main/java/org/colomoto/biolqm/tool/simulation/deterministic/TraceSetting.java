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
        if (parameters == null || parameters.length < 1) {
            return;
        }

        StringBuffer tmp = new StringBuffer();

        for (int idx=0 ; idx<parameters.length ; idx++) {

            String s = parameters[idx];

            String next = null;
            if (s.length() == 2 && s.charAt(0) == '-') {
                while (++idx < parameters.length) {
                    String snext = parameters[idx];
                    if (snext.startsWith("-")) {
                        idx--;
                        break;
                    }

                    if (next == null) {
                        next = snext;
                    } else {
                        next += " " + snext;
                    }
                }

                switch (s.charAt(1)) {
                    case 'u':
                        parseUpdater(next);
                        continue;
                    case 'm':
                        parseMax(next);
                        continue;
                    case 'i':
                        parseInitialState(next);
                        continue;
                }
            }

            throw new RuntimeException("Unrecognized parameter: "+s);
        }
    }


    public void parseMax(String s) {
        max_steps = Integer.parseInt(s);
    }

    public void parseUpdater(String s) {
        if (s.equalsIgnoreCase("sequential")) {
            isSequential = true;
            grouping = null;
            return;
        }

        if (s.equalsIgnoreCase("synchronous")) {
            isSequential = false;
            grouping = null;
            return;
        }

        if (s.startsWith("sequential ")) {
            isSequential = true;
            grouping = new DeterministicGrouping(model, s.substring(11));
            return;
        }

        if (s.startsWith("priority ")) {
            isSequential = false;
            grouping = new DeterministicGrouping(model, s.substring(9));
            return;
        }

        throw new RuntimeException("Unrecognized updater: "+s);
    }

    public void parseInitialState(String s) {
        int n = s.length();
        if (n == model.getComponents().size()) {
            this.state = new byte[n];
            for (int i=0 ; i<n ; i++) {
                state[i] = (byte)Character.getNumericValue(s.charAt(i));
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
        if (grouping != null) {
            if (isSequential) {
                return grouping.getBlockSequentialUpdater();
            }
            return grouping.getPriorityUpdater();
        }

        if (isSequential) {
            return new SequentialUpdater(model);
        }
        return new SynchronousUpdater(model);
    }
}

