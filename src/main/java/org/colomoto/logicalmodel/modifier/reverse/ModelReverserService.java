package org.colomoto.logicalmodel.modifier.reverse;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.modifier.AbstractModelModifier;
import org.colomoto.logicalmodel.modifier.ModelModifier;
import org.colomoto.logicalmodel.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Wrap the reverser code into the ModelModifier interface.
 *
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class ModelReverserService extends AbstractModelModifier {

    public static final String ID = "rev";
    public static final String NAME = "model reverser";
    public static final String DESCR = "(no parameters)";

    public ModelReverserService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model, String parameters) {
        return getModifier(model);
    }

    /**
     * Shorthand to skip the parameter String.
     *
     * @param model
     * @return
     */
    public ModelModifier getModifier(LogicalModel model) {
        return new ModelReverser(model);
    }

    /**
     * Shorthand to skip the parameter String.
     *
     * @param model
     * @return
     */
    public LogicalModel getModifiedModel(LogicalModel model) {
        return getModifier(model).getModifiedModel();
    }

}
