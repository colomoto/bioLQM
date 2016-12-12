package org.colomoto.logicalmodel.modifier.reverse;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelModifier;

/**
 * Wrap the reverser code into the LogicalModelModifier interface.
 *
 * @author Aurelien Naldi
 */
public class ModelReverser implements LogicalModelModifier {

    @Override
    public LogicalModel apply(LogicalModel model) {
        ModelReverserImpl worker = new ModelReverserImpl(model);
        return worker.getModel();
    }

}
