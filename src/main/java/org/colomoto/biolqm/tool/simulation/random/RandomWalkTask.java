package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolTask;
import org.colomoto.biolqm.tool.simulation.InitialStateFactory;
import org.colomoto.biolqm.tool.simulation.UpdaterFactory;

public class RandomWalkTask extends AbstractToolTask<RandomWalkSimulation> {

    String updater_config = null;
    byte[] state = null;
    int max_steps = 1000;
    int seed = -1; 

    public RandomWalkTask(LogicalModel model) {
        super(model);
    }

    @Override
    public void setParameters(String[] parameters) {
        if (parameters == null || parameters.length < 1) {
            return;
        }

        StringBuffer tmp = new StringBuffer();

        for (int idx=0 ; idx<parameters.length ; idx++) {

            String s = parameters[idx].trim();
            if (s == null || s.length() < 1) {
                continue;
            }

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
                        updater_config = next;
                        continue;
                    case 'm':
                        parseMax(next);
                        continue;
                    case 's':
                        parseSeed(next);
                        continue;
                    case 'i':
                        this.state = InitialStateFactory.parseInitialState(this.model, next);
                        continue;
                }
            }

            throw new RuntimeException("Unrecognized parameter: "+s);
        }
    }


    public void parseMax(String s) {
        max_steps = Integer.parseInt(s);
    }

    public void parseSeed(String s) {
        seed = Integer.parseInt(s);
    }

    public RandomWalkSimulation getSimulation() {
        byte[] state = this.state;
        if (state == null) {
            state = new byte[model.getComponents().size()];
        }

        RandomUpdater updater = UpdaterFactory.getRandomUpdater(model, updater_config);
        if (this.seed >= 0) {
            updater.setSeed(this.seed);
        }
        return new RandomWalkSimulation(updater, state, max_steps);
    }

    @Override
    protected RandomWalkSimulation performTask() {
        return getSimulation();
    }

    @Override
    public void cli() {
        RandomWalkSimulation simulation = getSimulation();
        for (byte[] state: simulation) {
            for (byte b: state) {
                System.out.print(b);
            }
            System.out.println();
        }
    }
}
