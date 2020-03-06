package org.colomoto.biolqm;

import java.util.List;
import java.util.Map;

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
	List<NodeInfo> getComponents();

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
	 * Check if the model has extra component(s).
	 * If this returns true, then the getExtraComponents() and
	 * getExtraLogicalFunctions() methods should not return null.
	 *
	 * @return true is there is at least one extra component
	 */
	boolean hasExtraComponents();

	/**
	 * Compute the value of an extra component for a given state.
	 *
	 * @param componentIdx index of the component in the extra node order
	 * @param state value of components
	 * @return the computed value for this state
	 */
	byte getExtraValue(int componentIdx, byte[] state);

	/**
	 * Compute the value of all extra components for a given state.
	 *
	 * @param state value of components
	 * @param extra an array of the right size to fill
	 */
	void fillExtraValues(byte[] state, byte[] extra);

	/**
	 * Make a copy of this model.
	 * This will duplicate logical functions pointers, but not the actual MDD Factory.
	 * 
	 * @return a copy of this model.
	 */
	LogicalModel clone();

	LogicalModel clone(boolean keepExtra);


	/**
	 * Get a logical model with the same nodes but a different order for the core components.
	 * 
	 * @param neworder the desired ordered list of components
	 * @return a re-ordered view of the same model
	 */
	LogicalModel getView(List<NodeInfo> neworder);

    /**
     * Check if this model is Boolean.
     *
     * @return true if the model has no multivalued components, false otherwise.
     */
    boolean isBoolean();


    /**
     * Retrieve a component by name.
     * 
     * @param s_source the name of the component
     * @return the component or null if not found
     */
	NodeInfo getComponent(String s_source);

	/**
	 * Retrieve a component by name.
	 *
	 * @param s_source the name of the component
	 * @return the component or null if not found
	 */
	int getComponentIndex(String s_source);

	/**
	 * Get the Boolean - multi-valued mapping information for booleanized models.
	 * 
	 * @return a list of booleanized groups
	 */
	Map<String, NodeInfo[]> getBooleanizedMap();

	/**
	 * Check if this model has associated layout information.
	 * @return true if some layout information is available
	 */
	boolean hasLayout();

	/**
	 * Retrieve the associated layout information.
	 * @return the existing layout information. Create it if it does not exist.
	 */
	ModelLayout getLayout();
}
