package org.colomoto.biolqm.modifier;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.common.task.Task;

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

	default void setParameters(String[] parameters) {
		
	}

	default void setParameters(String parameters) {
		String[] t_parameters = parameters.split(" ");
		setParameters(t_parameters);
	}

}
