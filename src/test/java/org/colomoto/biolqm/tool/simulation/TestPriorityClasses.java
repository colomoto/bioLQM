package org.colomoto.biolqm.tool.simulation;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
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
		ModelGrouping mpc = new ModelGrouping(model,
				"A" + ModelGrouping.SEPGROUP +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E");
		assertEquals("A" + ModelGrouping.SEPGROUP +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E", mpc.toString());
		

		List<String> lTmp = new ArrayList<String>();
		lTmp.add("A");
		lTmp.add("B");
		mpc.incPriorities(0, 0, lTmp);
		assertEquals("A" + ModelGrouping.SEPCLASS +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E", mpc.toString());

		mpc.split(0, 0, "A");
		mpc.split(1, 3, "E");
		assertEquals("A[-]" + ModelGrouping.SEPVAR +
				"A[+]" + ModelGrouping.SEPCLASS +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E[-]"+ ModelGrouping.SEPVAR +
				"E[+]", mpc.toString());

		lTmp.clear();
		lTmp.add("A[+]");
		mpc.decGroup(0, 0, lTmp);
		assertEquals("A[-]" + ModelGrouping.SEPGROUP +
				"A[+]" + ModelGrouping.SEPCLASS +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E[-]"+ ModelGrouping.SEPVAR +
				"E[+]", mpc.toString());
		
		mpc.collapseAll();
		assertEquals("E" + ModelGrouping.SEPVAR +
				"A" + ModelGrouping.SEPVAR +
				"B" + ModelGrouping.SEPVAR +
				"C"+ ModelGrouping.SEPVAR +
				"D", mpc.toString());
	}
	
	@Test
	public void testPriorityClasses2() throws IOException {
		LogicalModel model = getOtherModel();
		model.getComponents().get(0).setInput(true);
		// One class Async
		ModelGrouping mpc = new ModelGrouping(model,
				"A" + ModelGrouping.SEPGROUP +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E");
		assertEquals("B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E", mpc.toString());
	}
	
	
	@Test
	public void testPriorityClassesInit2() throws IOException {
		LogicalModel model = getOtherModel();
		model.getComponents().get(0).setInput(true);
		// One class Async
		ModelGrouping mpc = new ModelGrouping(model,
				"A" + ModelGrouping.SEPGROUP +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E");
		
		mpc.addUpdater(0, 0, new SynchronousUpdater(model));		
		assertEquals("B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E", mpc.toString());
				
		ModelGrouping mpc2 = new ModelGrouping(model,
				"A" + ModelGrouping.SEPVAR +
				"B" + ModelGrouping.SEPVAR +
				"C" + ModelGrouping.SEPVAR +
				"D" + ModelGrouping.SEPVAR +
				"E");
				
		mpc2.addUpdater(0, 0, new RandomUpdaterWithRates(model));
		String aa = mpc2.toString();
		assertEquals(
				"B" + ModelGrouping.SEPVAR +
				"C" + ModelGrouping.SEPVAR +
				"D" + ModelGrouping.SEPVAR +
				"E" + ModelGrouping.SEPUPDATER +
				"RN[1.0,1.0,1.0,1.0]", mpc2.toString());	
	}		
}
