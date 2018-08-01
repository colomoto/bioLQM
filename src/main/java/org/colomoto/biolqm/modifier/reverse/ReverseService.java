package org.colomoto.biolqm.modifier.reverse;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModifierService;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.kohsuke.MetaInfServices;

/**
 * Wrap the reverser code into the ModelModifier interface.
 *
 * @author Aurelien Naldi
 */
@MetaInfServices(ModelModifierService.class)
public class ReverseService extends AbstractModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "reverse";
    private static final String NAME = "model reverser";
    private static final String DESCR = "(no parameters)";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public ReverseService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model, String parameters) {
        return getModifier(model);
    }

    /**
     * Shorthand to retrieve a modifier without requiring a parameter String.
     *
     * @param model the model to be reversed
     * @return a reverser instance for this model
     */
    public ModelModifier getModifier(LogicalModel model) {
        return new ReverseModifier(model);
    }

    /**
     * Shorthand to retrieve a modified model without requiring a parameter String.
     *
     * @param model the model to be reversed
     * @return the reversed model
     */
    public LogicalModel getModifiedModel(LogicalModel model) {
        return getModifier(model).getModifiedModel();
    }

}
