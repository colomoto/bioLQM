package org.colomoto.biolqm.tool.simulation;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.BlockSequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.PriorityUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.jupiter.api.Test;

public class TestUpdaters {

	private LogicalModel getModel() {
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
		
		return new LogicalModelImpl(vars, manager, functions);
	}
	
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
		MDDVariable ve = manager.getVariableForKey(vars.get(4));
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
	public void testAsynchronousUpdater() throws IOException {
		LogicalModel model = getModel();
		MultipleSuccessorsUpdater updater = new AsynchronousUpdater(model);
		byte[] state = {0,0,0};
		List<byte[]> successors = updater.getSuccessors(state);
		assertEquals(2, successors.size());

		byte[] next = successors.get(0);
		assertEquals(1, next[0]);
		assertEquals(0, next[1]);
		assertEquals(0, next[2]);

        next = successors.get(1);
        assertEquals(0, next[0]);
		assertEquals(1, next[1]);
		assertEquals(0, next[2]);

	}

	@Test
	public void testSynchronousUpdater() throws IOException {
		LogicalModel model = getModel();
        DeterministicUpdater updater = new SynchronousUpdater(model);
		byte[] state = {0,0,0};
        byte[] next = updater.getSuccessor(state);
		
		assertEquals(1, next[0]);
		assertEquals(1, next[1]);
		assertEquals(0, next[2]);

	}
	
	@Test
	public void testBlockSequentialUpdater() throws IOException {
		
		LogicalModel model = getOtherModel();
		
		// create the block sequential scheme
		String scheme = "A,C,D:B,E"; //{1,2,1,1,2};
		ModelGrouping grouping = new ModelGrouping(model, scheme);
        DeterministicUpdater updater = new BlockSequentialUpdater(grouping);
		byte[] state = {1,1,1,1,1};
        byte[] next = updater.getSuccessor(state);
		
		assertEquals(1, next[0]);
		assertEquals(0, next[1]);
		assertEquals(0, next[2]);
		assertEquals(0, next[3]);
		assertEquals(1, next[4]);

//		int[] scheme1 = {1,1,1,1,2};
		String scheme1 = "A,B,C,D:E";
		grouping = new ModelGrouping(model, scheme1);
		updater = new BlockSequentialUpdater(grouping);
        next = updater.getSuccessor(state);
		
		assertEquals(1, next[0]);
		assertEquals(1, next[1]);
		assertEquals(0, next[2]);
		assertEquals(0, next[3]);
		assertEquals(1, next[4]);

//		int[] scheme2 = {1,2,3,4,5};
		String scheme2 = "A:B:C:D:E";
		grouping = new ModelGrouping(model, scheme2);
		updater = new BlockSequentialUpdater(grouping);
        next = updater.getSuccessor(state);

        DeterministicUpdater updater1 = new SequentialUpdater(model);
        byte[] next1 = updater1.getSuccessor(state);

		assertEquals(next1[0], next[0]);
		assertEquals(next1[1], next[1]);
		assertEquals(next1[2], next[2]);
		assertEquals(next1[3], next[3]);
		assertEquals(next1[4], next[4]);
		
	}

	@Test
	public void testSequentialUpdater() throws IOException {
		LogicalModel model = getModel();
        DeterministicUpdater updater = new SequentialUpdater(model);
		byte[] state = {0,0,0};
        byte[] next = updater.getSuccessor(state);
		
		assertEquals(1, next[0]);
		assertEquals(1, next[1]);
		assertEquals(1, next[2]);
	}

	@Test
	public void testCustomSequentialUpdater() throws IOException {
		LogicalModel model = getModel();
		int[] order = {1,2,0};
        DeterministicUpdater updater = new SequentialUpdater(model, order);
		byte[] state = {0,0,0};
        byte[] next = updater.getSuccessor(state);

		assertEquals(1, next[0]);
		assertEquals(1, next[1]);
		assertEquals(0, next[2]);
	}
	
	@Test
	public void testPriorityUpdater() throws IOException {
		LogicalModel model = getOtherModel();
		// One class Sync
		ModelGrouping mpc = new ModelGrouping(model,
				"A" + ModelGrouping.SEPVAR +
				"B" + ModelGrouping.SEPVAR +
				"C" + ModelGrouping.SEPVAR +
				"D" + ModelGrouping.SEPVAR +
				"E");
		
		PriorityUpdater updater = new PriorityUpdater(model, mpc);
		byte[] state = {1,1,0,1,0};
		List<byte[]> lNext = updater.getSuccessors(state);
		assertEquals(1, lNext.size());
		assertEquals(1, lNext.get(0)[0]);
		assertEquals(0, lNext.get(0)[1]);
		assertEquals(0, lNext.get(0)[2]);
		assertEquals(0, lNext.get(0)[3]);
		assertEquals(1, lNext.get(0)[4]);
		
		// One class Async
		mpc = new ModelGrouping(model,
				"A" + ModelGrouping.SEPGROUP +
				"B" + ModelGrouping.SEPGROUP +
				"C" + ModelGrouping.SEPGROUP +
				"D" + ModelGrouping.SEPGROUP +
				"E");
		updater = new PriorityUpdater(model, mpc);
		lNext = updater.getSuccessors(state);
		assertEquals(3, lNext.size());

		// Two class s[B-E+] s[AB+CDE-]
		mpc = new ModelGrouping(model,
				"B[-]" + ModelGrouping.SEPVAR +
				"E[+]" + ModelGrouping.SEPCLASS +
				"A" + ModelGrouping.SEPVAR +
				"B[+]" + ModelGrouping.SEPVAR +
				"C" + ModelGrouping.SEPVAR +
				"D" + ModelGrouping.SEPVAR +
				"E[-]");
		updater = new PriorityUpdater(model, mpc);
		lNext = updater.getSuccessors(state);
		assertEquals(1, lNext.size());
		assertEquals(1, lNext.get(0)[0]);
		assertEquals(0, lNext.get(0)[1]);
		assertEquals(0, lNext.get(0)[2]);
		assertEquals(1, lNext.get(0)[3]);
		assertEquals(1, lNext.get(0)[4]);
	}
}
