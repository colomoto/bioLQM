package org.colomoto.biolqm.modifier;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.ModelTask;

/**
 * Generic interface for modifications of logical models.
 * A modifier can be a generic modifier (output removal)
 * or a model-specific modifier (perturbation, reduction)
 *
 * @author Aurelien Naldi
 */
public interface ModelModifier extends ModelTask {

//    /**
//     * Apply the modifier and retrieve the new model
//     *
//     * @return a (new) LogicalModel where the modifications have been applied
//     */
//    default LogicalModel getModifiedModel() throws Exception {
//        return call();
//    }

}
