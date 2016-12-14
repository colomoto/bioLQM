package org.colomoto.logicalmodel.modifier.booleanize;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.modifier.AbstractModelModifierService;
import org.colomoto.logicalmodel.modifier.ModelModifier;
import org.colomoto.logicalmodel.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Wrap the booleanizer code into the ModelModifier interface.
 *
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class ModelBooleanizerService extends AbstractModelModifierService {

    public static final String ID = "booleanize";
    public static final String NAME = "model booleanizer";
    public static final String DESCR = "(no parameters)";

    public ModelBooleanizerService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model, String parameters) {
        return getModifier(model);
    }

    public ModelModifier getModifier(LogicalModel model) {
        return new ModelBooleanizer(model);
    }

    public LogicalModel getModifiedModel(LogicalModel model) {
        return getModifier(model).getModifiedModel();
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
