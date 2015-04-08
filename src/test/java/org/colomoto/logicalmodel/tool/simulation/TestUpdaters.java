package org.colomoto.logicalmodel.tool.simulation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.tool.simulation.updater.AsynchronousUpdater;
import org.colomoto.logicalmodel.tool.simulation.updater.BlockSequentialUpdater;
import org.colomoto.logicalmodel.tool.simulation.updater.PriorityClasses;
import org.colomoto.logicalmodel.tool.simulation.updater.PriorityUpdater;
import org.colomoto.logicalmodel.tool.simulation.updater.SequentialUpdater;
import org.colomoto.logicalmodel.tool.simulation.updater.SynchronousUpdater;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.Assert;
import org.junit.Test;

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
		Assert.assertEquals(2, successors.size());

		byte[] next = successors.get(0);
		Assert.assertEquals(1, next[0]);
		Assert.assertEquals(0, next[1]);
		Assert.assertEquals(0, next[2]);

        next = successors.get(1);
        Assert.assertEquals(0, next[0]);
		Assert.assertEquals(1, next[1]);
		Assert.assertEquals(0, next[2]);

	}

	@Test
	public void testSynchronousUpdater() throws IOException {
		LogicalModel model = getModel();
        DeterministicUpdater updater = new SynchronousUpdater(model);
		byte[] state = {0,0,0};
        byte[] next = updater.getSuccessor(state);
		
		Assert.assertEquals(1, next[0]);
		Assert.assertEquals(1, next[1]);
		Assert.assertEquals(0, next[2]);

	}
	
	@Test
	public void testBlockSequentialUpdater() throws IOException {
		
		LogicalModel model = getOtherModel();
		
		// create the block sequential scheme
		int[] scheme = {1,2,1,1,2};
        DeterministicUpdater updater = new BlockSequentialUpdater(model,scheme);
		byte[] state = {1,1,1,1,1};
        byte[] next = updater.getSuccessor(state);
		
		Assert.assertEquals(1, next[0]);
		Assert.assertEquals(0, next[1]);
		Assert.assertEquals(0, next[2]);
		Assert.assertEquals(0, next[3]);
		Assert.assertEquals(1, next[4]);

		int[] scheme1 = {1,1,1,1,2};
		updater = new BlockSequentialUpdater(model,scheme1);
        next = updater.getSuccessor(state);
		
		Assert.assertEquals(1, next[0]);
		Assert.assertEquals(1, next[1]);
		Assert.assertEquals(0, next[2]);
		Assert.assertEquals(0, next[3]);
		Assert.assertEquals(1, next[4]);

		int[] scheme2 = {1,2,3,4,5};
		updater = new BlockSequentialUpdater(model,scheme2);
        next = updater.getSuccessor(state);

        DeterministicUpdater updater1 = new SequentialUpdater(model);
        byte[] next1 = updater1.getSuccessor(state);

		Assert.assertEquals(next1[0], next[0]);
		Assert.assertEquals(next1[1], next[1]);
		Assert.assertEquals(next1[2], next[2]);
		Assert.assertEquals(next1[3], next[3]);
		Assert.assertEquals(next1[4], next[4]);
		
	}

	@Test
	public void testSequentialUpdater() throws IOException {
		LogicalModel model = getModel();
        DeterministicUpdater updater = new SequentialUpdater(model);
		byte[] state = {0,0,0};
        byte[] next = updater.getSuccessor(state);
		
		Assert.assertEquals(1, next[0]);
		Assert.assertEquals(1, next[1]);
		Assert.assertEquals(1, next[2]);
	}

	@Test
	public void testCustomSequentialUpdater() throws IOException {
		LogicalModel model = getModel();
		int[] order = {1,2,0};
        DeterministicUpdater updater = new SequentialUpdater(model, order);
		byte[] state = {0,0,0};
        byte[] next = updater.getSuccessor(state);

		Assert.assertEquals(1, next[0]);
		Assert.assertEquals(1, next[1]);
		Assert.assertEquals(0, next[2]);
	}
	
	@Test
	public void testPriorityUpdater() throws IOException {
		LogicalModel model = getOtherModel();
		PriorityClasses pcs = new PriorityClasses();
		int[] pc = {0,0,1,0,2,0,3,0,4,0};
		
		// One class Sync
		pcs.add(pc, true);
		PriorityUpdater updater = new PriorityUpdater(model, pcs);
		byte[] state = {1,1,0,1,0};
		List<byte[]> lNext = updater.getSuccessors(state);
		Assert.assertEquals(1, lNext.size());
		Assert.assertEquals(1, lNext.get(0)[0]);
		Assert.assertEquals(0, lNext.get(0)[1]);
		Assert.assertEquals(0, lNext.get(0)[2]);
		Assert.assertEquals(0, lNext.get(0)[3]);
		Assert.assertEquals(1, lNext.get(0)[4]);
		
		// One class Async
		pcs = new PriorityClasses();
		pcs.add(pc, false);
		updater = new PriorityUpdater(model, pcs);
		lNext = updater.getSuccessors(state);
		Assert.assertEquals(3, lNext.size());

		// Two class s[B-E+] s[AB+CDE-]
		pcs = new PriorityClasses();
		int[] pc1 = {1,-1,4,1};
		pcs.add(pc1, true);
		int[] pc2 = {0,0,1,1,2,0,3,0,4,-1};
		pcs.add(pc2, true);
		updater = new PriorityUpdater(model, pcs);
		lNext = updater.getSuccessors(state);
		Assert.assertEquals(1, lNext.size());
		Assert.assertEquals(1, lNext.get(0)[0]);
		Assert.assertEquals(0, lNext.get(0)[1]);
		Assert.assertEquals(0, lNext.get(0)[2]);
		Assert.assertEquals(1, lNext.get(0)[3]);
		Assert.assertEquals(1, lNext.get(0)[4]);

	}
}
