package org.colomoto.biolqm.tool.simulation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.HashSet;
import java.util.Iterator;


import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.updater.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.updater.SynchronousUpdater;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.Assert;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

class SimulationTestImpl extends MultipleSuccessorSimulation {

	public HashSet<byte[]> hs;

	public SimulationTestImpl(MultipleSuccessorsUpdater updater) {
		super(updater);
		hs = new HashSet<byte[]>();
	}

	@Override
	public void addState(byte[] state) {
		if ( !contains(state) ) {
			hs.add(state);
			enqueue(state);
		}
	}

	// Not optimal solution, but an hashcode of a byte[] is not well defined
	boolean contains(byte[] a) {
		boolean found = false;
		for(byte[] b: hs) {
			boolean idem = true;
			for(int i = 0; i < b.length; i++) {
				idem = idem && b[i] == a[i];
			}
			found = found || idem;
		}
		return found;
	}

	@Override
	public void addTransition(byte[] from, byte[] to) {
	}
}


public class TestSimulations {

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
	public void testMultipleSuccessorsSimulation() throws IOException {
		LogicalModel model = getModel();
		MultipleSuccessorsUpdater updater = new AsynchronousUpdater(model);
		byte[] state = {0, 0, 0};
		SimulationTestImpl simulation = new SimulationTestImpl(updater);

		simulation.addState(state);
		simulation.runSimulation();

		assertEquals(5, simulation.hs.size());
	}
	@Test
	public void testSingleSuccessorSimulation() throws IOException {
		LogicalModel model = getModel();
		DeterministicUpdater updater = new SynchronousUpdater(model);
		byte[] state = {0, 0, 0};
		byte[] state2 = {1, 1, 0};
		byte[] state3 = {1, 1, 1};
		SingleSuccessorSimulation simulation = new SingleSuccessorSimulation(updater, state, 100);
		
		Iterator<byte[]> it = simulation.iterator();
		
		Assert.assertArrayEquals(state, it.next());
		Assert.assertArrayEquals(state2, it.next());
		Assert.assertArrayEquals(state3, it.next());

	}
}
