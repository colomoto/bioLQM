package org.colomoto.biolqm.modifier.perturbation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModifierService;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Service to apply perturbations to Logical Models.
 * This service will wrap the perturbation objects into the ModelModifierService API
 *
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class PerturbationService extends AbstractModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "perturbation";
    private static final String NAME = "model perturbation";
    private static final String DESCR = "Example: Node1%0 Node2%1";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public PerturbationService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model, String parameters) {
        return new PerturbationModifier(model, parameters);
    }
}
