package org.colomoto.logicalmodel.io.antlr;

import java.util.Stack;

/**
 * Stack to reconstruct a parsed logical expression.
 * For now it will just rebuild a String, but it will be updated to build a real function object afterwards.
 *
 * @author Aurelien Naldi
 */
public class ExpressionStack {

    private Stack<String> stack = new Stack<String>();

    public void operator(Operator op) {
        String r = stack.pop();
        String l = stack.pop();

        stack.add( "("+l+ " "+op +" " + r+")");
    }

    public void not() {
        String c = stack.pop();
        stack.add( "NOT " + c);
    }

    public void value(Value v) {
        stack.add( v.toString());
    }

    public void ident(String name) {
        stack.add( name);
    }

    public String done() {
        if (stack.size() != 1) {
            throw new RuntimeException("The stack is messed-up");
        }

        return stack.pop();
    }
}
