package org.colomoto.logicalmodel.io.antlr;

import org.colomoto.mddlib.logicalfunction.FunctionNode;
import org.colomoto.mddlib.logicalfunction.OperandFactory;
import org.colomoto.mddlib.logicalfunction.ValueNode;
import org.colomoto.mddlib.logicalfunction.operators.AndOperatorFactory;
import org.colomoto.mddlib.logicalfunction.operators.NotOperatorFactory;

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
                result = AndOperatorFactory.FACTORY.getNode(stack);
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
