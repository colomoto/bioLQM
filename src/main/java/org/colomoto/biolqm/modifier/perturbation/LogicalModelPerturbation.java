package org.colomoto.biolqm.modifier.perturbation;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

/**
 * Common interface for perturbations.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelPerturbation {

	/**
	 * Apply the perturbation(s) directly to a state, i.e., restricts the values
	 * of the state in the valid range of the perturbation(s).
	 *
	 * @param state      the state to be restricted
	 * @param components the ordered list of components in the state
	 */
	void restrictValues(byte[] state, List<NodeInfo> components);

	/**
	 * Apply the perturbation to a model directly (do not create a modified model).
	 * This can be used directly when we do not need to preserve the original model.
	 * It is also convenient for multiple perturbations, to avoid creating intermediate models.
	 *
	 * @param model the modified model
	 */
	void update(LogicalModel model);

	/**
	 * Test if the perturbation affects the function of a specific node
	 *
	 * @param node a component of the model
	 * @return true if component is affected by this perturbation
	 */
	boolean affectsNode(NodeInfo node);

	/**
	 * Apply the perturbation.
	 *
	 * @param model the model to modify
	 * @return the modified model after applying the perturbation
	 */
	LogicalModel apply(LogicalModel model);

	/**
	 * @return a simple serialised description of the perturbation
	 */
	String getStringRepresentation();

}
