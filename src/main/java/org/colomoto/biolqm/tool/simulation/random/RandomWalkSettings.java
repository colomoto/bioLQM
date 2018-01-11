package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.ToolSettings;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicSimulation;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.CompleteUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.ordering.DeterministicGrouping;

public class RandomWalkSettings extends ToolSettings {

    boolean isComplete = false;
    byte[] state = null;
    int max_steps = 1000;

    public RandomWalkSettings(LogicalModel model) {
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
        if (s.equalsIgnoreCase("complete")) {
            isComplete = true;
            return;
        }

        if (s.equalsIgnoreCase("asynchronous")) {
            isComplete = false;
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

    public RandomWalkSimulation getSimulation() {
        byte[] state = this.state;
        if (state == null) {
            state = new byte[model.getComponents().size()];
        }

        RandomUpdater updater = getUpdater();
        return new RandomWalkSimulation(updater, state, max_steps);
    }

    public RandomUpdater getUpdater() {
        MultipleSuccessorsUpdater updater;
        if (isComplete) {
            updater = new CompleteUpdater(model);
        } else {
            updater = new AsynchronousUpdater(model);
        }
        return new RandomUpdaterWrapper(updater);
    }
}

