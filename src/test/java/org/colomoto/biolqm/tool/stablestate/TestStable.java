package org.colomoto.biolqm.tool.stablestate;

import org.colomoto.biolqm.LQMScriptLauncher;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ReferenceModels;
import org.junit.Test;

import java.io.IOException;

public class TestStable {


    @Test
    public void testBasicStable() throws IOException {
        LQMScriptLauncher lqm = new LQMScriptLauncher(null);
        StableStateTool stool = (StableStateTool) lqm.getTool("stable");

        String[] refModels = ReferenceModels.getNames();
        for (String name: refModels) {
            LogicalModel model = ReferenceModels.getModel(name);
            StableStateList list = stool.getMDD(model);
        }
    }
}
