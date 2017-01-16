package org.colomoto.biolqm.modifier.perturbation;

import org.colomoto.biolqm.LogicalModel;

/**
 * Base class for Perturbations.
 * It only provides a generic apply method: clone the model and update the clone.
 * 
 * @author Aurelien Naldi
 */
public abstract class AbstractPerturbation implements LogicalModelPerturbation {

	@Override
	public LogicalModel apply(LogicalModel model) {
		LogicalModel newModel = model.clone();
		update(newModel);
		
		return newModel;
	}
}
