package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModifierService;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Orchestrator for model reduction tools
 *
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class ReductionService extends AbstractModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "reduce";
    private static final String NAME = "model reduction";
    private static final String DESCR = ":fixed | :duplicate | :output";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public ReductionService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ReductionModifier getModifier(LogicalModel model, String parameters) {
        return new ReductionModifier(model, parameters);
    }

    public ReductionModifier getModifier(LogicalModel model) {
        return new ReductionModifier(model);
    }

}
