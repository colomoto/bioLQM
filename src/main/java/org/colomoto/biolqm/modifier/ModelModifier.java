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

}
