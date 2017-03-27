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

    /** The identifier used to retrieve this service by name */
    public static final String ID = "reduce";
    private static final String NAME = "model reduction";
    private static final String DESCR = "[TODO: flags for fixed,duplicates,outputs]";

    /**
     * Public constructor which should only be used for service discovery.
     */
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
