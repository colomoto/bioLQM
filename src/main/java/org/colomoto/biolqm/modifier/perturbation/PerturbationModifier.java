package org.colomoto.biolqm.modifier.perturbation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;

public class PerturbationModifier implements ModelModifier {

    private final LogicalModel model;
    private final LogicalModelPerturbation perturbation;

    public PerturbationModifier(LogicalModel model, LogicalModelPerturbation perturbation) {
        this.model = model;
        this.perturbation = perturbation;
    }

    @Override
    public LogicalModel getModifiedModel() {
        LogicalModel result = model;

        if (perturbation != null) {
            result = perturbation.apply(result);
        }

        return result;
    }
}
