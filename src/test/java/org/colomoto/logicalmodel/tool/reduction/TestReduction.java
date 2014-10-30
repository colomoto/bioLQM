package org.colomoto.logicalmodel.tool.reduction;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.ReferenceModels;
import org.junit.Test;

public class TestReduction {

	@Test
	public void testCoreReduction() throws IOException {
		LogicalModel model = ReferenceModels.getModel("simpleFunctions.txt");

		List<NodeInfo> core = model.getNodeOrder();
		List<NodeInfo> extra = model.getExtraComponents();
		
		int nbCore = core.size();
		int nbExtra = extra.size();

		assertEquals(5, nbCore);
		assertEquals(0, nbExtra);

		ModelReducer reducer = new ModelReducer(model);
		int[] toRemove = new int[] {3,4};
		for (int idx: toRemove) {
			reducer.remove(idx);
		}
		LogicalModel reducedModel = reducer.getModel();
		
		core = reducedModel.getNodeOrder();
		extra = reducedModel.getExtraComponents();
		
		assertEquals(nbCore-toRemove.length, core.size());
		assertEquals(nbExtra+toRemove.length, extra.size());
	}

	@Test
	public void testOutputReduction() throws IOException {
		checkOutputReduction("simpleFunctions.txt", 1, 1);
	}
	
	public void checkOutputReduction(String name, int expectedOutputs, int expectedPseudoOutputs) throws IOException {
		LogicalModel model = ReferenceModels.getModel(name);
		List<NodeInfo> core = model.getNodeOrder();
		List<NodeInfo> extra = model.getExtraComponents();
		int nbCore = core.size();
		int nbExtra = extra.size();

		assertEquals(5, nbCore);
		assertEquals(0, nbExtra);

		ModelReducer reducer = new ModelReducer(model);
		int removed = reducer.removePseudoOutputs();
		assertEquals(expectedPseudoOutputs, removed);
		
		LogicalModel reducedModel = reducer.getModel();
		core = reducedModel.getNodeOrder();
		extra = reducedModel.getExtraComponents();
		
		int eRemoved = expectedOutputs + expectedPseudoOutputs;
		assertEquals(nbCore-eRemoved, core.size());
		assertEquals(nbExtra+eRemoved, extra.size());
	}

    @Test
    public void testFixedPropagation() throws IOException {
        LogicalModel model = ReferenceModels.getModel("propagate_fixed.txt");

        FixedComponentRemover remover = new FixedComponentRemover();

        LogicalModel newModel = remover.apply(model);

        int[] oldFunctions = model.getLogicalFunctions();
        int[] functions = newModel.getLogicalFunctions();
        assertEquals(1, functions[1]);
        assertEquals(0, functions[2]);
        assert(oldFunctions[3] != functions[3]);
        assertEquals(oldFunctions[4], functions[4]);
    }
}
