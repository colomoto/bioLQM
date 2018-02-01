package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LQMScriptLauncher;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ReferenceModels;
import org.junit.Test;

import java.io.IOException;

public class TestFixpoint {


    @Test
    public void testBasicStable() throws IOException {
        LQMScriptLauncher lqm = new LQMScriptLauncher(null);
        FixpointTool stool = (FixpointTool) lqm.getTool("fixpoints");

        String[] refModels = ReferenceModels.getNames();
        for (String name: refModels) {
            LogicalModel model = ReferenceModels.getModel(name);
            FixpointList list = stool.getMDD(model);
        }
    }
}
