package org.colomoto.biolqm.modifier.perturbation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModelModifierService;
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
public class PerturbationService extends AbstractModelModifierService {

    public static final String ID = "perturbation";
    public static final String NAME = "model perturbation";
    public static final String DESCR = "[TODO!!]";

    public PerturbationService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model, String parameters) {
        return new PerturbationModifier(model, getPerturbationFromString(model, parameters));
    }

    private static LogicalModelPerturbation getPerturbationFromString(LogicalModel model, String parameters) {

        // FIXME: parse the perturbation definition String and construct it

        return null;
    }

    // TODO: add convenient programmatic access to generate perturbations from scripts and Java code
}
