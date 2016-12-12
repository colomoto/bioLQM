package org.colomoto.logicalmodel.modifier.booleanize;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelModifier;

/**
 * Wrap the booleanizer code into the LogicalModelModifier interface.
 *
 * @author Aurelien Naldi
 */
public class ModelBooleanizer implements LogicalModelModifier {

    @Override
    public LogicalModel apply(LogicalModel model) {
        Booleanizer worker = new Booleanizer(model);
        return worker.getModel();
    }
}
