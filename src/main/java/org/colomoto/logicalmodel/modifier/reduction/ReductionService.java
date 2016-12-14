package org.colomoto.logicalmodel.modifier.reduction;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.modifier.ModelModifier;
import org.colomoto.logicalmodel.services.Service;
import org.mangosdk.spi.ProviderFor;

/**
 * Entry point for the reduction tools in script mode.
 *
 * @author Aurelien Naldi
 */
@ProviderFor(Service.class)
public class ReductionService implements Service {

    private static final String ID="reduction";

    @Override
    public String getID() {
        return ID;
    }

    @Override
    public String getName() {
        return "Model reduction: remove components";
    }


    public LogicalModel removeOutputs(LogicalModel model) {
        OutputSimplifier simplifier = new OutputSimplifier(model);
        return simplifier.getModifiedModel();
    }

    public LogicalModel removeDuplicates(LogicalModel model) {
        ModelModifier simplifier = new DuplicateRemover(model);
        return simplifier.getModifiedModel();
    }
}
