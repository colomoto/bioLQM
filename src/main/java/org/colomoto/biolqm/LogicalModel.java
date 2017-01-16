package org.colomoto.biolqm;

import java.util.List;

import org.colomoto.mddlib.MDDManager;


/**
 * A LogicalModel is a ready to use object containing all necessary information to perform simulations and analysis,
 * without the editing facilities provided by a full RegulatoryGraph.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModel {

	/**
	 * Get the MDD factory holding logical functions for this model.
	 * @return the MDD factory in which logical function are stored.
	 */
	MDDManager getMDDManager();

	/**
	 * Get the list of core nodes in this model.
	 * Logical functions for these nodes (in the same order) can be obtained with the
	 * <code>getLogicalFunctions()</code> method.
	 * 
	 * @return the list of nodeInfo objects for core components.
	 */
	List<NodeInfo> getNodeOrder();
	
	/**
	 * Get the logical function of core components in this model.
	 * The order used is the same as for <code>getNodeOrder()</code>.
	 * These functions are just identifiers, actual functions are stored in
	 * the MDD factory provided by <code>getMDDManager</code>.
	 * 
	 * @return the list of logical function identifiers for core components
	 */
	int[] getLogicalFunctions();

	/**
	 * Get the list of extra components in this model.
	 * Extra components have logical functions, but do not regulate any component,
	 * they are thus NOT considered as variables in the MDDManager.
	 * Logical functions for these nodes (in the same order) can be obtained with the
	 * <code>getExtraLogicalFunctions()</code> method.
	 * 
	 * @return the list of nodeInfo objects for extra components.
	 */
	List<NodeInfo> getExtraComponents();
	
	/**
	 * Get the logical function of extra components in this model.
	 * The order used is the same as for <code>getExtraNodes()</code>.
	 * These functions are just identifiers, actual functions are stored in
	 * the MDD factory provided by <code>getMDDManager()</code>.
	 * 
	 * @return the list of logical function identifiers for extra components
	 */
	int[] getExtraLogicalFunctions();

	/**
	 * Compute the target value of a core component for a given state.
	 * 
	 * @param componentIdx index of the component in the core node order
	 * @param state value of components
	 * @return the target value reached for this state
	 */
	byte getTargetValue(int componentIdx, byte[] state);
	
	/**
	 * Compute the value of an extra component for a given state.
	 * 
	 * @param componentIdx index of the component in the extra node order
	 * @param state value of components
	 * @return the computed value for this state
	 */
	byte getExtraValue(int componentIdx, byte[] state);
	
	/**
	 * Make a copy of this model.
	 * This will duplicate logical functions pointers, but not the actual MDD Factory.
	 * 
	 * @return a copy of this model.
	 */
	LogicalModel clone();

	
	/**
	 * Get a logical model with the same nodes but a different order for the core components.
	 * 
	 * @param neworder
	 * @return
	 */
	LogicalModel getView(List<NodeInfo> neworder);

    /**
     * Check if this model is Boolean.
     *
     * @return true if the model has no multivalued components, false otherwise.
     */
    boolean isBoolean();
}
