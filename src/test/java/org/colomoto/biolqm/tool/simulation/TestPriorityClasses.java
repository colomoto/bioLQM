package org.colomoto.biolqm.tool.simulation;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping.VarInfo;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.widgets.UpdaterFactoryModelGrouping;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.Assert;
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
		assertEquals("A" + ModelGrouping.SEPVAR +
				"B" + ModelGrouping.SEPVAR +
				"C" + ModelGrouping.SEPVAR +
				"D"+ ModelGrouping.SEPVAR +
				"E", mpc.toString());
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
				"RN[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]", mpc2.toString());
		
		mpc2.addUpdater(0, 0, new RandomUpdaterWithRates(model, new Double[] {1.0,1.0,3.0,4.0,6.0,8.0,5.0,1.0,1.0,1.0}));
		assertEquals(
				"B" + ModelGrouping.SEPVAR +
				
				"C" + ModelGrouping.SEPVAR +
				"D" + ModelGrouping.SEPVAR +
				"E" + ModelGrouping.SEPUPDATER +
				"RN[1.0,1.0,3.0,4.0,6.0,8.0,5.0,1.0,1.0,1.0]", mpc2.toString());
		
		;	
	}
	
	//@Test
	public void testPriorityClassesInitError() throws IOException {
		// LogicalModel, Map<Rank, Map<List<GroupVars>,updater>>
		
		LogicalModel model = getOtherModel();
		model.getComponents().get(0).setInput(true);

		List<VarInfo> group1 = new ArrayList<VarInfo>();
		List<VarInfo> group2 = new ArrayList<VarInfo>();
		List<VarInfo> group3 = new ArrayList<VarInfo>();

		
		group1.add(new VarInfo(0, 0, model));
		group1.add(new VarInfo(1, -1, model));
		
		group2.add(new VarInfo(1, 1, model));
		group2.add(new VarInfo(2, 0, model));
		
		group3.add(new VarInfo(3, -1, model));
		group3.add(new VarInfo(3, 1, model));
		group3.add(new VarInfo(4, 0, model));
		
		Map<List<VarInfo>, LogicalModelUpdater> groupsRank0 = 
				new HashMap<List<VarInfo>, LogicalModelUpdater>();
		
		Map<List<VarInfo>, LogicalModelUpdater> groupsRank1 = 
				new HashMap<List<VarInfo>, LogicalModelUpdater>();

		groupsRank0.put(group1, UpdaterFactoryModelGrouping.getUpdater(model, "Synchronous"));
		groupsRank0.put(group2, UpdaterFactoryModelGrouping.getUpdater(model,
				"Random non uniform"));
		groupsRank1.put(group3, UpdaterFactoryModelGrouping.getUpdater(model, "Random uniform"));
		
		
		Map<Integer, Map<List<VarInfo>, LogicalModelUpdater>> ranks = 
				new HashMap<Integer, Map<List<VarInfo>, LogicalModelUpdater>>();
		
		ranks.put(0, groupsRank0);
		ranks.put(1, groupsRank1);
		

		try {
			ModelGrouping mpc = new ModelGrouping(model, ranks);
		} catch (Exception e) {
			e.printStackTrace();
		}
	
	}
	@Test
	public void testPriorityClassesInit() throws IOException {
		// LogicalModel, Map<Rank, Map<List<GroupVars>,updater>>
		
		LogicalModel model = getOtherModel();
		model.getComponents().get(0).setInput(true);

		List<VarInfo> group1 = new ArrayList<VarInfo>();
		List<VarInfo> group2 = new ArrayList<VarInfo>();
		List<VarInfo> group3 = new ArrayList<VarInfo>();

		
		group1.add(new VarInfo(1, -1, model));
		
		group2.add(new VarInfo(1, 1, model));
		group2.add(new VarInfo(2, 0, model));
		
		group3.add(new VarInfo(3, -1, model));
		group3.add(new VarInfo(3, 1, model));
		group3.add(new VarInfo(4, 0, model));
		
		Map<List<VarInfo>, LogicalModelUpdater> groupsRank0 = 
				new HashMap<List<VarInfo>, LogicalModelUpdater>();
		
		Map<List<VarInfo>, LogicalModelUpdater> groupsRank1 = 
				new HashMap<List<VarInfo>, LogicalModelUpdater>();

		groupsRank0.put(group1, UpdaterFactoryModelGrouping.getUpdater(model, "Synchronous"));
		groupsRank0.put(group2, UpdaterFactoryModelGrouping.getUpdater(model,
				"Random non uniform"));
		groupsRank1.put(group3, UpdaterFactoryModelGrouping.getUpdater(model, "Random uniform"));
		
		
		Map<Integer, Map<List<VarInfo>, LogicalModelUpdater>> ranks = 
				new HashMap<Integer, Map<List<VarInfo>, LogicalModelUpdater>>();
		
		ranks.put(0, groupsRank0);
		ranks.put(1, groupsRank1);
		ModelGrouping mpc = null;

		try {
			mpc = new ModelGrouping(model, ranks);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// since Groups were passed in a Map object, the order of appearance 
		// differs between B[-] and B[+]
		Assert.assertTrue(
				mpc.toString().equals("B[+],C$RN[null,null,null,1.0,1.0,1.0,null,null,null,null]/"
						+ "B[-]:D[-],D[+],E$RU") ||
				mpc.toString().equals("B[-]/B[+],C$RN[null,null,null,1.0,1.0,1.0,null,null,null,null]"
						+ ":D[-],D[+],E$RU")
				);

	}

}







