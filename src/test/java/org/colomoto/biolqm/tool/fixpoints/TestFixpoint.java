package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LQMScriptLauncher;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ReferenceModels;
import org.junit.Test;

public class TestFixpoint {


    @Test
    public void testBasicStable() throws Exception {
        LQMScriptLauncher lqm = new LQMScriptLauncher(null);
        FixpointService stool = (FixpointService) lqm.getTool("fixpoints");

        String[] refModels = ReferenceModels.getNames();
        for (String name: refModels) {
            LogicalModel model = ReferenceModels.getModel(name);
            FixpointList list = stool.getTask(model).call();
        }
    }
}
