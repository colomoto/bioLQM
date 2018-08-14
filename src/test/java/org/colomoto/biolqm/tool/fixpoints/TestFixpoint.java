package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ReferenceModels;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.junit.jupiter.api.Test;

public class TestFixpoint {


    @Test
    public void testBasicStable() throws Exception {
        FixpointService stool = LQMServiceManager.get(FixpointService.class);

        String[] refModels = ReferenceModels.getNames();
        for (String name: refModels) {
            LogicalModel model = ReferenceModels.getModel(name);
            FixpointList list = stool.getTask(model).call();
        }
    }
}
