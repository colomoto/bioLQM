package org.colomoto.logicalmodel.modifier.perturbation;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.modifier.AbstractModelModifierService;
import org.colomoto.logicalmodel.modifier.ModelModifier;
import org.colomoto.logicalmodel.modifier.ModelModifierService;
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


class PerturbationModifier implements ModelModifier {

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
