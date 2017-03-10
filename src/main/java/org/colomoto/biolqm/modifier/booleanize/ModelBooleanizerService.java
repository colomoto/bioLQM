package org.colomoto.biolqm.modifier.booleanize;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModelModifierService;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * A service for model booleanization.
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
