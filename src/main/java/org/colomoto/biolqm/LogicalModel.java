package org.colomoto.biolqm;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import org.colomoto.mddlib.MDDManager;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.colomoto.biolqm.metadata.AnnotationModule;
import org.colomoto.biolqm.metadata.NodeInfoPair;
import org.colomoto.biolqm.metadata.annotations.JsonReader;
import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.metadata.constants.Index;

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
	 * 
	 * @return the existing layout information. Create it if it does not exist.
	 */
	ModelLayout getLayout();
	
	/**
	 * Create a Metadata object for a node of the model
	 *
	 * @param node the node you want to annotate
	 * @return the Metadata object you created for the node
	 * @throws Exception 
	 */
	Metadata createMetadataOfNode(NodeInfo node) throws Exception;
	/**
	 * Create a Metadata object for an edge of the model
	 *
	 * @param edge the edge you want to annotate
	 * @return the Metadata object you created for the edge
	 * @throws Exception 
	 */
	Metadata createMetadataOfEdge(NodeInfoPair edge) throws Exception;
	
	/**
	 * Retrieve the Metadata object of the model
	 * @return the existing Metadata of the model
	 */
	Metadata getMetadataOfModel();
	
	/**
	 * Check if a metadata object exists for a node
	 *
	 * @param node the node you want to check
	 * @return true if it exists, false otherwise
	 */	
	boolean isSetMetadataOfNode(NodeInfo node);
	
	/**
	 * Check if a metadata object exists for an edge
	 *
	 * @param edge the edge you want to check
	 * @return true if it exists, false otherwise
	 */	
	boolean isSetMetadataOfEdge(NodeInfoPair edge);
	
	/**
	 * Retrieve the Metadata object of the node
	 * 
	 * @param node the node you want to annotate
	 * @return the existing Metadata of the node. Create it if it does not exist.
	 * @throws Exception 
	 */
	Metadata getMetadataOfNode(NodeInfo node) throws Exception;
	
	/**
	 * Retrieve the Metadata object of an edge
	 * 
	 * @param edge the edge you want to annotate
	 * @return the existing Metadata of the node. Create it if it does not exist.
	 * @throws Exception 
	 */
	Metadata getMetadataOfEdge(NodeInfoPair edge) throws Exception;
	
	/**
	 * Retrieve the Metadata object of an edge
	 * 
	 * @param node1 the source node of the edge
	 * @param node2 the target node of the edge
	 * @return the existing Metadata of the node. Create it if it does not exist.
	 * @throws Exception 
	 */
	Metadata getMetadataOfEdge(NodeInfo node1, NodeInfo node2) throws Exception;
	
	/**
	 * Export all the metadata of the model in a structured json file
	 * 
	 * @param filename the name of the json file
	 * @throws JSONException 
	 */
	void exportMetadata(String filename);
	
	/**
	 * Import a structured json file to populate the metadata of the model
	 * 
	 * @param filename the name of the json file
	 */
	void importMetadata(String filename);
	
	/**
	 * Modify the annotation module of the model
	 */
	void setAnnotationModule(AnnotationModule newAnnotationModule);
	
	/**
	 * Retrieve the annotation module of the model
	 * 
	 * @return the annotation module
	 */
	AnnotationModule getAnnotationModule();
}
