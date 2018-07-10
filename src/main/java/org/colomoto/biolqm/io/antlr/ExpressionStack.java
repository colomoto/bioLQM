package org.colomoto.biolqm.io.antlr;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDOperator;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.logicalfunction.FunctionNode;
import org.colomoto.mddlib.logicalfunction.OperandFactory;
import org.colomoto.mddlib.logicalfunction.ValueNode;
import org.colomoto.mddlib.logicalfunction.operators.AndOperatorFactory;
import org.colomoto.mddlib.logicalfunction.operators.NotOperatorFactory;
import org.colomoto.mddlib.logicalfunction.operators.OrOperatorFactory;
import org.colomoto.mddlib.operators.OverwriteOperator;

import java.util.List;
import java.util.Map;
import java.util.Stack;

/**
 * Stack to reconstruct a parsed logical expression.
 * This takes a series of "signal" denoting operands and operators (sent by the parser)
 * and constructs a proper logical function tree from it.
 * It handles the shared parts of function parsing from different formats.
 *
 * @author Aurelien Naldi
 */
public class ExpressionStack {

    /**
     * Build a LogicalModel from a list of nodes and parsed functions.
     * This method should be shared between parsers!
     *
     * @param operandFactory factory providing the MDD manager and operands for these nodes
     * @param nodes the list of nodes
     * @param var2function the parsed function for each node
     * 
     * @return the corresponding logical model
     */
    public static LogicalModel constructModel(OperandFactory operandFactory, List<NodeInfo> nodes, Map<NodeInfo,FunctionNode> var2function) {

        MDDManager manager = operandFactory.getMDDManager();

        int[] functions = new int[nodes.size()];
        for (int i=0 ; i<functions.length ; i++) {
            FunctionNode node = var2function.get( nodes.get(i));
            if (node == null) {
                functions[i] = 0;
            } else {
                functions[i] = node.getMDD( manager);
            }
        }
        return new LogicalModelImpl(nodes, manager, functions);
    }

    public static LogicalModel constructMVModel(OperandFactory operandFactory, List<NodeInfo> nodes, Map<NodeInfo,List<Assignment>> var2assign) {

        // Prepare the corresponding MDD manager
        MDDVariableFactory mvf = new MDDVariableFactory();
        for (NodeInfo ni: nodes) {
            mvf.add(ni, (byte)(ni.getMax()+1));
        }
        MDDManager manager = MDDManagerFactory.getManager(mvf, 10);

        int[] functions = new int[nodes.size()];
        for (int i=0 ; i<functions.length ; i++) {
            NodeInfo ni = nodes.get(i);
            List<Assignment> asgs = var2assign.get( ni);
            functions[i] = 0;
            for (Assignment asg: asgs) {
                if (asg == null) {
                    continue;
                }

                int overMDD = asg.condition.getMDD(manager);
                MDDOperator op = OverwriteOperator.getOverwriteAction(asg.value);
                int old = functions[i];
                functions[i] = op.combine(manager, old, overMDD);
                manager.free(overMDD);
                manager.free(old);
            }
        }
        return new LogicalModelImpl(nodes, manager, functions);
    }


    private Stack<FunctionNode> stack = new Stack<FunctionNode>();
    private final OperandFactory operandFactory;

    public ExpressionStack(OperandFactory operandFactory) {
        this.operandFactory = operandFactory;
    }

    public void operator(Operator op) {

        FunctionNode result = null;
        switch (op) {
            case AND:
                result = AndOperatorFactory.FACTORY.getNode(stack);
                break;
            case OR:
                result = OrOperatorFactory.FACTORY.getNode(stack);
                break;
        }
        if (result == null) {
            // ???
            return;
        }
        stack.add( result);
    }

    public void not() {
        FunctionNode node = stack.pop();
        if (node == ValueNode.TRUE) {
            stack.add( ValueNode.FALSE);
        } else if (node == ValueNode.FALSE) {
            stack.add( ValueNode.TRUE);
        } else {
            stack.add(NotOperatorFactory.FACTORY.getNode(node));
        }
    }

    public void value(Value v) {
        switch (v) {
            case TRUE:
                stack.add(ValueNode.TRUE);
                break;
            case FALSE:
                stack.add(ValueNode.FALSE);
                break;
        }
    }

    public void ident(String name) {
        FunctionNode node = operandFactory.createOperand(name);
        if (node == null) {
            throw new RuntimeException("Operand could not be created");
        }
        stack.add( node);
    }

    public void ident(String name, int threshold) {
        FunctionNode node = operandFactory.createOperand(name, threshold);
        if (node == null) {
            throw new RuntimeException("Operand could not be created");
        }
        stack.add( node);
    }

    public FunctionNode done() {
        if (stack.size() != 1) {
            throw new RuntimeException("The stack is messed-up");
        }

        return stack.pop();
    }

    public void clear() {
        stack.clear();
    }
}
