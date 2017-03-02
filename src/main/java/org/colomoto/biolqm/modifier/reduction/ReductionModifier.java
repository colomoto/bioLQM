package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;

public class ReductionModifier implements ModelModifier {

    private final LogicalModel model;
    private final ReductionSettings settings;

    public ReductionModifier(LogicalModel model, ReductionSettings settings) {
        this.model = model;
        this.settings = settings;
    }

    @Override
    public LogicalModel getModifiedModel() {
        LogicalModel result = model;

        if (settings.handleFixed) {
            result = FixedComponentRemover.reduceFixed(result, settings.purgeFixed);
        }

        if (settings.handleDuplicates) {
            result = DuplicateRemover.removeDuplicateComponents(result);
        }

        ModelReducer reducer = new ModelReducer(model);
        if (settings.handleOutputs) {
            reducer.removePseudoOutputs();
        }

        return reducer.getModel();
    }
}
