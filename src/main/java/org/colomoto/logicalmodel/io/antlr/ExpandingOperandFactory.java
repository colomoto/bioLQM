package org.colomoto.logicalmodel.io.antlr;

import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.logicalfunction.AbstractOperand;
import org.colomoto.mddlib.logicalfunction.OperandFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Discover a list of operands as we parse
 *
 * @author Aurelien Naldi
 */
public class ExpandingOperandFactory implements OperandFactory {

    private final Map<String, SimpleOperand<NodeInfo>> operandMap = new HashMap<String, SimpleOperand<NodeInfo>>();
    private final List<NodeInfo> operands;

    private MDDManager ddmanager = null;

    /**
     * Discover a list of operands as we parse
     *
     * @param operands
     */
    public ExpandingOperandFactory(List<NodeInfo> operands) {
        this.operands = operands;
        int i=0;
        for (NodeInfo obj: operands) {
            SimpleOperand<NodeInfo> operand = new SimpleOperand<NodeInfo>(obj, i);
            i++;
            operandMap.put(operand.toString(), operand);
        }
    }

    @Override
    public boolean verifOperandList(List<String> list) {
        return true;
    }

    @Override
    public AbstractOperand createOperand(String name) {
        return operandMap.get(name);
    }

    @Override
    public MDDManager getMDDManager() {
        if (ddmanager == null) {

            ddmanager = MDDManagerFactory.getManager(operands, 2);
        }
        return ddmanager;
    }
}

class SimpleOperand<T> extends AbstractOperand {

    T object;
    int variable;

    public SimpleOperand(T object, int variable) {
        this.object = object;
        this.variable = variable;
    }
    @Override
    public String toString(boolean par) {
        return object.toString();
    }
    @Override
    public T getMDDVariableKey() {
        return object;
    }
}