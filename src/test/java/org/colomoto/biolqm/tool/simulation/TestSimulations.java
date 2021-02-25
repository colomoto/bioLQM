package org.colomoto.biolqm.tool.simulation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;


import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicSimulation;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorSimulation;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomWalkSimulation;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;


import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

	// cria modelo lógico com três nós + três funções 
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
	
	private LogicalModel getComponents() {
		// build a list of variables and functions for a model
		List<NodeInfo> vars = new ArrayList<NodeInfo>();
		vars.add(new NodeInfo("A"));
		vars.add(new NodeInfo("B"));
		vars.add(new NodeInfo("C"));
		vars.add(new NodeInfo("D"));
		vars.add(new NodeInfo("E"));
		
		// (keys to be used, number of possible values) in the MDD
		MDDManager manager = new MDDStoreImpl(vars, 2);
		
		int[] functions = new int[vars.size()];
		
		MDDVariable va = manager.getVariableForKey(vars.get(0));
		MDDVariable vb = manager.getVariableForKey(vars.get(1));
		MDDVariable vc = manager.getVariableForKey(vars.get(2));
		MDDVariable vd = manager.getVariableForKey(vars.get(3));
		
		// (x,x) left child, right child 
		// returns id of node
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
		// ASYN
		MultipleSuccessorsUpdater updater = new AsynchronousUpdater(model);
		byte[] state = {0, 0, 0};
		SimulationTestImpl simulation = new SimulationTestImpl(updater);

		simulation.addState(state);
		simulation.runSimulation();

		assertEquals(5, simulation.hs.size());
		
	}
		
//	@Test
	public void testSingleSuccessorSimulation() throws IOException {
		LogicalModel model = getModel();
		// SYNCHROUNOUS 
		DeterministicUpdater updater = new SynchronousUpdater(model);
		byte[] state = {1, 0, 1};
//		byte[] state2 = {1, 1, 0};
//		byte[] state3 = {1, 1, 1};
		// 100 - max steps
		DeterministicSimulation simulation = new DeterministicSimulation(updater, state, 100);
		
		Iterator<byte[]> it = simulation.iterator();		
		
		// 000 > 110 > 111
//		assertArrayEquals(new byte[] {0,0,0}, it.next());
//		assertArrayEquals(new byte[] {1,1,0}, it.next());
//		assertArrayEquals(new byte[] {1,1,1}, it.next());
		
		// 010/100 > 110 > 111
//		assertArrayEquals(new byte[] {0,1,0}, it.next());
//		assertArrayEquals(new byte[] {1,1,0}, it.next());
//		assertArrayEquals(new byte[] {1,1,1}, it.next());

		// 110 > 111
//		assertArrayEquals(new byte[] {1,1,0}, it.next());
//		assertArrayEquals(new byte[] {1,1,1}, it.next());
		// next doesnt exist
//		assertArrayEquals(new byte[] {1,1,1}, it.next());

		// 001 > 110 > 111
//		assertArrayEquals(new byte[] {0,0,1}, it.next());
//		assertArrayEquals(new byte[] {1,1,0}, it.next());
//		assertArrayEquals(new byte[] {1,1,1}, it.next());
		
		// 101/011 > 110 > 111 
		assertArrayEquals(new byte[] {1,0,1}, it.next());
		assertArrayEquals(new byte[] {1,1,0}, it.next());
		assertArrayEquals(new byte[] {1,1,1}, it.next());

	}
	
	@Test
	public void testSingleSuccessorSimulationComp() throws IOException {
		LogicalModel model = getComponents();
		// SYNCHROUNOUS 
		DeterministicUpdater updater = new SynchronousUpdater(model);
//		byte[] state = {0, 0, 0, 0, 0};
		
//		byte[] state = {1, 1, 1, 1, 1};
		byte[] state = {1, 1, 0, 1, 0};

		
		// 100 - max steps
		DeterministicSimulation simulation = new DeterministicSimulation(updater, state, 100);
		
		Iterator<byte[]> it = simulation.iterator();
		
		//while(it.hasNext()) {
		//	System.out.println(Arrays.toString(it.next()));
		//}
		
	}
	@Test
	public void testRandomAsynSimulation() throws IOException {
	LogicalModel model = getComponents();
	
	byte[] state = {0, 0, 0, 0, 0};
	// C and D
	byte[][] states = {{0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}};
	int[] count = new int[2];
	
	RandomUpdaterWrapper updater = new RandomUpdaterWrapper(new AsynchronousUpdater(model));
		
	RandomWalkSimulation simulation = new RandomWalkSimulation(updater, state, 100);
	
    int size = model.getComponents().size();
    int[] simUpdates = new int[size];
    Set<Integer> updatables = new HashSet<Integer>();
	Iterator<byte[]> it = simulation.iterator();

    int simRuns = 10000;  

    for (int run = 0; run < simRuns ; run++) {
           it = simulation.iterator();
           it.next();
           byte[] successor = it.next();
           if (successor == null) {
                  break;
           }

           int idx = getIdxChange(state, successor);
           updatables.add(idx);
           simUpdates[idx] += 1;

    }
           double prob = 1.0/updatables.size();
           double[] probs = new double[simUpdates.length];
           for (int i = 0; i < probs.length; i++)
                  probs[i] = (simUpdates[i] == 0) ? 0 : prob;
                             
           for (int compIdx = 0; compIdx < size; compIdx++) {
                  assertTrue(simUpdates[compIdx] >= simRuns * probs[compIdx] * 0.9
                                && simUpdates[compIdx] <= simRuns * probs[compIdx] * 1.1);
           }

    }
	
	  @Test

      public void testRandomRatesSimulation() throws IOException {
		  
      LogicalModel model = getComponents();
      byte[] state = {0, 0, 0, 0, 0};
      // C and D
      int[] count = new int[2];
      double[] rates = new double[] {0.4,0.1,0.1,0.2,0.2};


      RandomUpdaterWithRates updater = new RandomUpdaterWithRates(model,rates);
      RandomWalkSimulation simulation = new RandomWalkSimulation(updater, state, 100);

      Iterator<byte[]> it = simulation.iterator();

      int size = model.getComponents().size();
      int[] simUpdates = new int[size];
      Set<Integer> updatables = new HashSet<Integer>();

      int simRuns = 10000;  

      for (int run = 0; run < simRuns ; run++) {

             it = simulation.iterator();
             it.next();
             byte[] successor = it.next();
             if (successor == null) {
                    break;
             }

             int idx = getIdxChange(state, successor);
             updatables.add(idx);
             simUpdates[idx] += 1;
      }

             double[] newRates = new double[size];
             double sum = 0.0;
             // sum of rates of updatable components
             for (int id : updatables) {
                    sum += rates[id];
             }

             // calculate new rates
             for (int id : updatables) {
                    newRates[id] = rates[id]/sum;
             }

             for (int compIdx = 0; compIdx < size; compIdx++) {
                    assertTrue(simUpdates[compIdx] >= simRuns * newRates[compIdx] * 0.9
                                  && simUpdates[compIdx] <= simRuns * newRates[compIdx] * 1.1);		
             }
      }
     

      private int getIdxChange(byte[] state1, byte[] state2) {

             int idx = 0;
             boolean foundchange = false;
             while (!foundchange && idx < state1.length) {
                    if (state2[idx] != state1[idx]) {
                           foundchange = true;
                    }
                    else {
                           idx += 1;
                    }                   
             }
             return idx;
      }

	
	
}

