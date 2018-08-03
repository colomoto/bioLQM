package org.colomoto.biolqm.modifier.perturbation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.ModelModifier;

import java.util.ArrayList;
import java.util.List;

public class PerturbationModifier implements ModelModifier {

    private final LogicalModel model;
    private LogicalModelPerturbation perturbation;

    public PerturbationModifier(LogicalModel model) {
        this.model = model;
    }

    @Override
    public LogicalModel getModifiedModel() {
        if (perturbation != null) {
            return perturbation.apply(model);
        }

        return model;
    }

    @Override
    public void setParameters(String[] parameters) {
        if (parameters.length == 1) {
            this.perturbation = getSimplePerturbationFromString(model, parameters[0]);
        }

        List<LogicalModelPerturbation> perturbations = new ArrayList<LogicalModelPerturbation>();
        for (String s: parameters) {
            LogicalModelPerturbation p = getSimplePerturbationFromString(model, s);
            if (p != null) {
                perturbations.add(p);
            }
        }

        if (perturbations.size() < 1) {
            this.perturbation = null;
        }
        if (perturbations.size() == 1) {
            this.perturbation = perturbations.get(0);
        }
        this.perturbation = new MultiplePerturbation<LogicalModelPerturbation>(perturbations);
    }

    /**
     * Parse and reconstruct a simple perturbation.
     *
     * @param model
     * @param parameters
     * @return
     */
    private static LogicalModelPerturbation getSimplePerturbationFromString(LogicalModel model, String parameters) {
        String[] params = parameters.split("%");
        if (params.length != 2) {
            //TODO: report error?
            return null;
        }
        String s_source = null;
        String s_target = params[0];
        int idx = s_target.indexOf(':');
        if (idx>=0) {
            s_source = s_target.substring(0, idx);
            s_target = s_target.substring(idx+1);
        }
        NodeInfo source = model.getComponent(s_source);
        NodeInfo target = model.getComponent(s_target);

        String restriction = params[1];

        idx = restriction.indexOf(':');
        int min = -1;
        int max = -1;
        if (idx >= 0) {
            min = Integer.parseInt(restriction.substring(0, idx));
            max = Integer.parseInt(restriction.substring(idx+1));
        } else {
            min = Integer.parseInt(restriction);
            max = min;
        }

        if (max < min) {
            min = max;
        }

        if (source == null) {
            if (min == max) {
                return new FixedValuePerturbation(target, min);
            }
            return new RangePerturbation(target, min, max);
        }

        if (min == max) {
            return new InteractionPerturbation(source, target, min);
        }

        // TODO: support range restriction on interactions?
        return null;
    }

}
