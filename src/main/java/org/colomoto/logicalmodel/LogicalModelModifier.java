package org.colomoto.logicalmodel;

/**
 * Generic interface for modifications of logical models.
 * A modifier can be a generic modifier (output removal)
 * or a model-specific modifier (perturbation, reduction)
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelModifier {

	/**
	 * Apply this modifier to a model and construct a resulting model.
	 * 
	 * @param model
	 * @return a new LogicalModel where the modifications have been applied
	 */
	LogicalModel apply(LogicalModel model);
}
