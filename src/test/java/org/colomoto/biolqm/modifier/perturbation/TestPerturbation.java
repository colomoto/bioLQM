package org.colomoto.biolqm.modifier.perturbation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.Assert;
import org.junit.Test;

public class TestPerturbation {

	@Test
	public void testInteractionPerturbation() throws IOException {

		// build a list of variables and functions for a model
		List<NodeInfo> vars = new ArrayList<NodeInfo>();
		vars.add(new NodeInfo("A"));
		vars.add(new NodeInfo("B"));
		vars.add(new NodeInfo("C"));
		
		MDDManager manager = new MDDStoreImpl(vars, 2);
		int[] functions = new int[vars.size()];
		functions[0] = 1;
		functions[1] = 1;
		MDDVariable va = manager.getVariableForKey(vars.get(0));
		MDDVariable vb = manager.getVariableForKey(vars.get(1));
		int fa = va.getNode(0, 1);
		int fb = vb.getNode(0, 1);
		functions[2] = MDDBaseOperators.AND.combine(manager, fa, fb);
		
		LogicalModel model = new LogicalModelImpl(vars, manager, functions);

		// create and apply a perturbation
		LogicalModelPerturbation p = new InteractionPerturbation(vars.get(1), vars.get(2), 1);
		LogicalModel pmodel= p.apply(model);
		int[] pfunctions = pmodel.getLogicalFunctions();
		
		Assert.assertEquals( pfunctions[0], functions[0]);
		Assert.assertEquals( pfunctions[1], functions[1]);
		Assert.assertEquals( pfunctions[2], fa);
	}
}
