package org.colomoto.logicalmodel.perturbation;

import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * A multiple perturbation is a list of perturbations all applied together.
 * 
 * @author Aurelien Naldi
 */
public class MultiplePerturbation extends AbstractPerturbation {

	private final List<LogicalModelPerturbation> perturbations;

	/**
	 * Create a multiple perturbation
	 * 
	 * @param perturbations list of perturbations to apply.
	 */
	public MultiplePerturbation(List<LogicalModelPerturbation> perturbations) {
		this.perturbations = perturbations;
	}
	
	@Override
	public void update(LogicalModel model) {
		for (LogicalModelPerturbation perturbation: perturbations) {
			perturbation.update(model);
		}
	}
}
