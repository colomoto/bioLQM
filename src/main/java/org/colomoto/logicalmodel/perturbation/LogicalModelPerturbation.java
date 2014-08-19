package org.colomoto.logicalmodel.perturbation;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelModifier;
import org.colomoto.logicalmodel.NodeInfo;

/**
 * Common interface for perturbations.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelPerturbation extends LogicalModelModifier {

	/**
	 * Apply the perturbation to a model directly (do not create a modified model).
	 * This can be used directly when we do not need to preserve the original model.
	 * It is also convenient for multiple perturbations, to avoid creating intermediate models.
	 * 
	 * @param model
	 */
	void update(LogicalModel model);

    /**
     * Test if the perturbation affects the function of a specific node
     *
     * @param node
     * @return true if node is affected by this perturbation
     */
    boolean affectsNode(NodeInfo node);

}
