package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolTask;
import org.colomoto.biolqm.tool.simulation.InitialStateFactory;
import org.colomoto.biolqm.tool.simulation.UpdaterFactory;

public class TraceTask extends AbstractToolTask<DeterministicSimulation> {

    String updater_config = null;
    byte[] state = null;
    int max_steps = 1000;
    int length = 100;

    public TraceTask(LogicalModel model) {
        super(model);
    }

    @Override
    public void setParameters(String[] parameters) {
        if (parameters == null) {
            return;
        }

        StringBuffer tmp = new StringBuffer();

        for (int idx=0 ; idx<parameters.length ; idx++) {

            String s = parameters[idx].trim();
            if (s == null || s.length() == 0) {
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
                    case 'l':
                        parseLength(next);
                        continue;
                    case 'i':
                        this.state = InitialStateFactory.parseInitialState(this.model, next);
                        continue;
                }
            }

            throw new RuntimeException("Unrecognized parameter: "+s);
        }
    }


    private void parseMax(String s) {
        max_steps = Integer.parseInt(s);
    }

    private void parseLength(String s) {
        length = Integer.parseInt(s);
    }

    public DeterministicSimulation getSimulation() {
        byte[] state = this.state;
        if (state == null) {
            state = new byte[model.getComponents().size()];
        }

        DeterministicUpdater updater = UpdaterFactory.getDeterministicUpdater(model, updater_config);
        return new DeterministicSimulation(updater, state, length, max_steps);
    }

    @Override
    protected DeterministicSimulation performTask() {
        return getSimulation();
    }

    @Override
    public void cli() {
        DeterministicSimulation simulation = getSimulation();
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

