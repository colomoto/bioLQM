package org.colomoto.biolqm.io.antlr;

import org.colomoto.mddlib.logicalfunction.FunctionNode;

public class Assignment {
    public final int value;
    public final FunctionNode condition;

    public Assignment(int value, FunctionNode condition) {
        this.value = value;
        this.condition = condition;
    }
}
