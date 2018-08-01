package org.colomoto.biolqm.helper.implicants;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ReferenceModels;
import org.colomoto.mddlib.MDDManager;
import org.junit.jupiter.api.Test;

public class TestImplicants {


    @Test
    public void testImplicants() throws Exception {
        for (String s: ReferenceModels.getNames()) {
            System.out.println("Primes for model: "+s);
            LogicalModel model = ReferenceModels.getModel(s);

            MDDManager ddmanager = model.getMDDManager();
            MDD2PrimeImplicants m2i = new MDD2PrimeImplicants(ddmanager);
            for (int f: model.getLogicalFunctions()) {
                m2i.getPrimes(f, 0);
                m2i.getPrimes(f, 1);
            }
        }
    }

}
