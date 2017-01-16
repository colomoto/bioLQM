package org.colomoto.biolqm.modifier;

import org.colomoto.biolqm.LogicalModel;

/**
 * Generic interface for modifications of logical models.
 * A modifier can be a generic modifier (output removal)
 * or a model-specific modifier (perturbation, reduction)
 * 
 * @author Aurelien Naldi
 */
public interface ModelModifier {

	/**
	 * Apply the modifier and retrieve the new model
	 * 
	 * @return a (new) LogicalModel where the modifications have been applied
	 */
	LogicalModel getModifiedModel();
}
