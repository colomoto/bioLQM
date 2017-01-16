package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModelModifierService;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Orchestrator for model reduction tools
 *
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class ModelReductionService extends AbstractModelModifierService {

    public static final String ID = "reduce";
    public static final String NAME = "model reduction";
    public static final String DESCR = "[TODO: flags for fixed,duplicates,outputs]";

    public ModelReductionService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model, String parameters) {
        ReductionSettings settings = getSettings(parameters);
        return getModifier(model, settings);
    }

    public ReductionSettings getSettings() {
        return new ReductionSettings();
    }

    public ReductionSettings getSettings(String parameters) {
        return new ReductionSettings(parameters);
    }

    public ModelModifier getModifier(LogicalModel model, ReductionSettings settings) {
        return new ReductionModifier(model, settings);
    }

}

class ReductionSettings {

    public boolean handleFixed = false;
    public boolean purgeFixed = false;

    public boolean handleDuplicates = false;

    public boolean handleOutputs = false;

    public ReductionSettings() {
        this("");
    }

    public ReductionSettings(String parameters) {
        String[] options = parameters.split(",");
        for (String o: options) {
            if ("fixed".equalsIgnoreCase(o)) {
                handleFixed = true;
            } else if ("duplicate".equalsIgnoreCase(o)) {
                handleDuplicates = true;
            } else if ("output".equalsIgnoreCase(o)) {
                handleOutputs = true;
            }
        }
    }

}

class ReductionModifier implements ModelModifier {

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
