package org.colomoto.logicalmodel.tool.reverse;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * Reverse a node according to a given variable.
 * This allows the construction of a model with the reversed
 * dynamical behaviour, for example to find backward reachable states.
 *
 * @author Aurelien Naldi
 */
public class ReverseOperation {

    private final MDDManager manager;

    public ReverseOperation(MDDManager manager) {
        this.manager = manager;
    }

    /**
     * Reverse a node.
     *
     * if the node does not contain the target variable,
     * it amounts to a regular negation.
     * On the target variable, the two children are swapped
     * in addition to being negated.
     *
     * @param var
     * @param node
     * @return
     */
    public int reverse(MDDVariable var, int node) {

        MDDVariable curvar = manager.getNodeVariable(node);
        if (var.after(curvar)) {
            // reverse all children and continue
            int n = curvar.nbval;
            if (n == 2) {
                int c0 = reverse(var, manager.getChild(node, 0));
                int c1 = reverse(var, manager.getChild(node, 1));
                int newNode = curvar.getNode(c0,c1);
                manager.free(c0);
                manager.free(c1);
                return newNode;
            }
            // this part could actually work if we want to reverse only some functions
            throw new RuntimeException("Reverse only applies to Boolean functions");
        }

        if (curvar == var) {
            // negate the children and swap them
            int c0 = manager.not( manager.getChild(node, 1));
            int c1 = manager.not( manager.getChild(node, 0));
            int newNode = curvar.getNode(c0,c1);
            manager.free(c0);
            manager.free(c1);
            return newNode;

        }

        // the target variable will not appear, use the internal negation
        return manager.not(node);
    }

}
