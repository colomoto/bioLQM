package org.colomoto.biolqm.helper.inferinteraction;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.jupiter.api.Test;

public class TestInteractionSearch {

	@Test
	public void test() {

		LogicalModel model = getSimpleModel();
		InteractionSearcher isearch = new InteractionSearcher(model);
		isearch.run();
	}

	public static LogicalModel getSimpleModel() {
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		byte max = 1;
		variables.add(new NodeInfo("v1", max));
		variables.add(new NodeInfo("v2", max));
		variables.add(new NodeInfo("v3", max));
		
		
		MDDManager ddmanager = MDDManagerFactory.getManager(variables, 5);
		MDDVariable[] ddVariables = ddmanager.getAllVariables();
		int[] functions = new int[3];
		
		int v1 = ddVariables[0].getNode(0, 1);
		int v2 = ddVariables[1].getNode(0, 1);
		int nv2 = ddVariables[1].getNode(1, 0);
		int v1nv2 = MDDBaseOperators.AND.combine(ddmanager, v1, nv2);
		int v1v2 = MDDBaseOperators.AND.combine(ddmanager, v1, v2);
		
		functions[0] = v1;
		functions[1] = v1nv2;
		functions[2] = v1v2;
		
		
		return new LogicalModelImpl(ddmanager, variables, functions, new ArrayList<NodeInfo>(), new int[0]);
	}

}
