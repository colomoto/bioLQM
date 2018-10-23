package org.colomoto.biolqm.tool.simulation;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.ModelPriorityClasses;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.jupiter.api.Test;

public class TestPriorityClasses {
	
	private LogicalModel getOtherModel() {
		// build a list of variables and functions for a model
		List<NodeInfo> vars = new ArrayList<NodeInfo>();
		vars.add(new NodeInfo("A"));
		vars.add(new NodeInfo("B"));
		vars.add(new NodeInfo("C"));
		vars.add(new NodeInfo("D"));
		vars.add(new NodeInfo("E"));
		
		MDDManager manager = new MDDStoreImpl(vars, 2);
		int[] functions = new int[vars.size()];
		MDDVariable va = manager.getVariableForKey(vars.get(0));
		MDDVariable vb = manager.getVariableForKey(vars.get(1));
		MDDVariable vc = manager.getVariableForKey(vars.get(2));
		MDDVariable vd = manager.getVariableForKey(vars.get(3));
		int fa = va.getNode(0, 1);
		int fna = va.getNode(1, 0);
		int fb = vb.getNode(0, 1);
		int fnb = vb.getNode(1,0);
		int fc = vc.getNode(0, 1);
		int fd = vd.getNode(0, 1);

		functions[0] = fa;
		functions[1] = MDDBaseOperators.AND.combine(manager, fa, fc);
		functions[2] = fnb;
		functions[3] = fna;
		functions[4] = MDDBaseOperators.OR.combine(manager, fb, fd);
		
		return new LogicalModelImpl(vars, manager, functions);
	}
	

	@Test
	public void testPriorityClasses1() throws IOException {
		LogicalModel model = getOtherModel();
		// One class Async
		ModelPriorityClasses mpc = new ModelPriorityClasses(model,
				"A" + ModelPriorityClasses.SEPGROUP + 
				"B" + ModelPriorityClasses.SEPGROUP + 
				"C" + ModelPriorityClasses.SEPGROUP + 
				"D" + ModelPriorityClasses.SEPGROUP + 
				"E");
		assertEquals("A" + ModelPriorityClasses.SEPGROUP + 
				"B" + ModelPriorityClasses.SEPGROUP + 
				"C" + ModelPriorityClasses.SEPGROUP + 
				"D" + ModelPriorityClasses.SEPGROUP + 
				"E", mpc.toString());

		List<String> lTmp = new ArrayList<String>();
		lTmp.add("A");
		lTmp.add("B");
		mpc.incPriorities(0, 0, lTmp);
		assertEquals("A" + ModelPriorityClasses.SEPCLASS + 
				"B" + ModelPriorityClasses.SEPGROUP + 
				"C" + ModelPriorityClasses.SEPGROUP + 
				"D" + ModelPriorityClasses.SEPGROUP + 
				"E", mpc.toString());

		mpc.split(0, 0, "A");
		mpc.split(1, 3, "E");
		assertEquals("A[-]" + ModelPriorityClasses.SEPVAR +
				"A[+]" + ModelPriorityClasses.SEPCLASS + 
				"B" + ModelPriorityClasses.SEPGROUP + 
				"C" + ModelPriorityClasses.SEPGROUP + 
				"D" + ModelPriorityClasses.SEPGROUP + 
				"E[-]"+ ModelPriorityClasses.SEPVAR + 
				"E[+]", mpc.toString());

		lTmp.clear();
		lTmp.add("A[+]");
		mpc.decGroup(0, 0, lTmp);
		assertEquals("A[-]" + ModelPriorityClasses.SEPGROUP +
				"A[+]" + ModelPriorityClasses.SEPCLASS + 
				"B" + ModelPriorityClasses.SEPGROUP + 
				"C" + ModelPriorityClasses.SEPGROUP + 
				"D" + ModelPriorityClasses.SEPGROUP + 
				"E[-]"+ ModelPriorityClasses.SEPVAR + 
				"E[+]", mpc.toString());
		
		mpc.collapseAll();
		assertEquals("E" + ModelPriorityClasses.SEPVAR + 
				"A" + ModelPriorityClasses.SEPVAR + 
				"B" + ModelPriorityClasses.SEPVAR + 
				"C"+ ModelPriorityClasses.SEPVAR + 
				"D", mpc.toString());
	}
	
	@Test
	public void testPriorityClasses2() throws IOException {
		LogicalModel model = getOtherModel();
		model.getComponents().get(0).setInput(true);
		// One class Async
		ModelPriorityClasses mpc = new ModelPriorityClasses(model,
				"A" + ModelPriorityClasses.SEPGROUP + 
				"B" + ModelPriorityClasses.SEPGROUP + 
				"C" + ModelPriorityClasses.SEPGROUP + 
				"D" + ModelPriorityClasses.SEPGROUP + 
				"E");
		assertEquals("B" + ModelPriorityClasses.SEPGROUP + 
				"C" + ModelPriorityClasses.SEPGROUP + 
				"D" + ModelPriorityClasses.SEPGROUP + 
				"E", mpc.toString());
	}
}
