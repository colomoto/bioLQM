package org.colomoto.biolqm.modifier.submodel;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

import java.util.*;

/**
 * Given a list of component names, this modifier will extract a sub-model containing:
 * - the given list of components,
 * - all dependent components of these nodes (involved in the logical functions) as input components (if they are not fixed values).
 *
 * NB: current implementation doesn't manage extra components/functions.
 *
 * @author Celine Hernandez
 */
public class SubmodelModifier extends BaseModifier {

    /**
     * Model from which the user wants to extract a sub-part.
     */
    private final LogicalModel model;

    /**
     * List of Node that the user want to extract from the global model.
     */
    private ArrayList<NodeInfo> compToExtract;

    
    public SubmodelModifier(LogicalModel model) {
        this.model = model;
        this.compToExtract = null;
    }

    
    @Override
    public LogicalModel performTask() {
        if (this.compToExtract.size() == 0) {
            return model;
        }

        // Model manager
        MDDManager mddManager = model.getMDDManager();
        // All logical functions
        int[] functions = model.getLogicalFunctions();
        // All components of the model
        List<NodeInfo> allModelComponents = model.getComponents();

        // Number of components
        int nb_components = allModelComponents.size();


        // Find which Nodes need to be extracted

        // Var to store whether a component will be part of the sub-model
        boolean[] belongsToSubmodel = new boolean[nb_components];

        // For each member of the model
        for (int i = 0; i < nb_components; i++) {

            // Is it in the list to extract?
            if (compToExtract.contains(allModelComponents.get(i))) {
    
                // Add current node, in case it would be an output.
                belongsToSubmodel[i] = true;
                
                // If yes, find which component has an influence on its value
                // Get a list of Boolean values for all components of the model
                boolean[] isDV = mddManager.collectDecisionVariables(functions[i]);

                // Merge with belongsToSubmodel
                for (int j = 0; j < nb_components; j++) {
                    belongsToSubmodel[j] = (belongsToSubmodel[j] || isDV[j]);
                }
            }
        }


        // Create the sub-model

        // Count components
        int new_components = 0;
        for (int i = 0; i < nb_components; i++) {
            if (belongsToSubmodel[i]) {
                new_components++;
            }
        }

        // New nodes list
        List<NodeInfo> submodelNodes = new ArrayList<>(new_components);
        // New functions list
        int[] submodelFunctions = new int[new_components];

        // Index in new lists
        int i_new = 0;

        for (int i = 0; i < nb_components; i++) {
            if (belongsToSubmodel[i]) {

                // Include Node
                NodeInfo currentNodeInfo = allModelComponents.get(i);
                submodelNodes.add(currentNodeInfo);

                // Include functions :

                // Will this node become an input components of the future sub-model?
                //  (ie, we mustn't modify Nodes with fixed values (leafs in the MDD) nor Nodes already listed to be extracted (these keep their function)).
                // If yes, its logical function needs to be adjusted in the sub-model and become an auto-regulation.
                if (!mddManager.isleaf(functions[i]) && !compToExtract.contains(currentNodeInfo)) {

                    // Modify associated function if current node will become an input
                    // Convert function into auto-regulations
                    MDDVariable var = mddManager.getVariableForKey(currentNodeInfo);
                    int autoreg;
                    // Simple case of a Boolean variable
                    if (var.nbval == 2) {
                        autoreg = var.getNode(0, 1);
                    }
                    // Multi-valued variable
                    else {
                        // Need to create a table of values before the function
                        int[] vals = new int[var.nbval];
                        for (int val = 0; val < var.nbval; val++) {
                            vals[val] = val;
                        }
                        autoreg = var.getNode(vals);
                    }

                    // Assign new function
                    submodelFunctions[i_new++] = autoreg;
                }
                else {
                    // Include available function
                    submodelFunctions[i_new++] = functions[i];
                }
            }
        }

        // Finally, create the sub-model
        return new LogicalModelImpl(submodelNodes, mddManager, submodelFunctions);
    }

    @Override
    public void setParameters(String[] parameters) {
        ArrayList<NodeInfo> validNodes = new ArrayList<>();
        ArrayList<String> invalidNodes = new ArrayList<>();
    
        // Check that parameters can be found in the model
        for (String p : parameters) {
            NodeInfo ninf = model.getComponent(p);
            if (ninf == null) {
                invalidNodes.add(p);
            } else {
                validNodes.add(ninf);
            }

        }

        // Warn about the presence of invalid nodes.
        if (invalidNodes.size() > 0) {
            System.out.println("Warning: Invalid nodes will be ignored: " + String.join(", ", invalidNodes));
        }
        
        // Warn about empty parameters and behaviour.
        if (parameters.length == 0 || validNodes.size() == 0) {
            System.out.println("Warning: No parameters to extract. Full model will be returned.");
        }
    
        // Warn about the presence of duplicated elements
        if (new LinkedHashSet<>(Arrays.asList(parameters)).size() < parameters.length ) {
            System.out.println("Warning: Duplicated parameters.");
        }
    
        compToExtract = validNodes;
    }
    
    
}
