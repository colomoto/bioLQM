package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.ordering.DeterministicGrouping;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class TestOrdering {

    private LogicalModel getModel() {
        // build a list of variables and functions for a model
        List<NodeInfo> vars = new ArrayList<NodeInfo>();
        vars.add(new NodeInfo("A"));
        vars.add(new NodeInfo("B"));
        vars.add(new NodeInfo("C"));

        MDDManager manager = new MDDStoreImpl(vars, 2);
        MDDVariable va = manager.getVariableForKey(vars.get(0));
        MDDVariable vb = manager.getVariableForKey(vars.get(1));

        int[] functions = new int[vars.size()];
        int fa = va.getNode(0, 1);
        int fb = vb.getNode(0, 1);
        functions[0] = 1;
        functions[1] = fa;
        functions[2] = MDDBaseOperators.OR.combine(manager, fa, fb);

        return new LogicalModelImpl(vars, manager, functions);
    }


    @Test
    public void testOrdering() throws IOException {

        LogicalModel model = getModel();
        String s_grouping = "C[+] C[-],A B[+],B[-]";
        DeterministicGrouping grouping = new DeterministicGrouping(model, s_grouping);

        DeterministicUpdater updater = grouping.getBlockSequentialUpdater();
        byte[] state = {0,0,1};

        byte[] next = updater.getSuccessor(state);
        assertEquals(1, next[0]);
        assertEquals(1, next[1]);
        assertEquals(0, next[2]);

        next = updater.getSuccessor(next);
        assertEquals(1, next[0]);
        assertEquals(1, next[1]);
        assertEquals(1, next[2]);

        next = updater.getSuccessor(next);
        assertNull(next);



        updater = grouping.getPriorityUpdater();
        state = new byte[] {0,0,1};

        next = updater.getSuccessor(state);
        assertEquals(1, next[0]);
        assertEquals(0, next[1]);
        assertEquals(0, next[2]);

        next = updater.getSuccessor(next);
        assertEquals(1, next[0]);
        assertEquals(0, next[1]);
        assertEquals(1, next[2]);

        next = updater.getSuccessor(next);
        assertEquals(1, next[0]);
        assertEquals(1, next[1]);
        assertEquals(1, next[2]);

        next = updater.getSuccessor(next);
        assertNull(next);
    }

}
