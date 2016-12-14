package org.colomoto.logicalmodel.modifier.booleanize;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.modifier.AbstractModelModifier;
import org.colomoto.logicalmodel.modifier.ModelModifier;
import org.colomoto.logicalmodel.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Wrap the booleanizer code into the ModelModifier interface.
 *
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class ModelBooleanizerService extends AbstractModelModifier {

    public static final String ID = "rev";
    public static final String NAME = "model modifier";

    public ModelBooleanizerService() {
        super(ID, NAME);
    }

    public ModelModifier getModifier(LogicalModel model, String parameters) {
        return new ModelBooleanizer(model);
    }

}

class ModelBooleanizer implements ModelModifier {

    private final LogicalModel model;

    protected ModelBooleanizer(LogicalModel model) {
        this.model = model;
    }

    @Override
    public LogicalModel getModifiedModel() {
        Booleanizer worker = new Booleanizer(model);
        return worker.getModel();
    }
}
