package org.colomoto.biolqm.tool.simulation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
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
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.PriorityUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping.VarInfo;
import org.colomoto.biolqm.tool.simulation.random.RandomWalkSimulation;
import org.colomoto.biolqm.widgets.UpdaterFactoryModelGrouping;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;

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
	
	private LogicalModel getSimpleModel() {
		// build a list of variables and functions for a model
		List<NodeInfo> vars = new ArrayList<NodeInfo>();
		vars.add(new NodeInfo("A"));
		vars.add(new NodeInfo("B"));
		vars.add(new NodeInfo("C"));
		
		MDDManager manager = new MDDStoreImpl(vars, 2);
		int[] functions = new int[vars.size()];

		
		MDDVariable va = manager.getVariableForKey(vars.get(0));
		MDDVariable vb = manager.getVariableForKey(vars.get(1));
		MDDVariable vc = manager.getVariableForKey(vars.get(2));
		int fa = va.getNode(0, 1);
		int fb = vb.getNode(0, 1);
		int fc = vc.getNode(0, 1);

		functions[0] = MDDBaseOperators.AND.combine(manager, fb, fc);
		functions[1] = fa;
		functions[2] = fb;
		
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
	
	private ModelGrouping getMpcModel() throws IOException {
		// LogicalModel, Map<Rank, Map<List<GroupVars>,updater>>
		
		LogicalModel model = getComponents();
		model.getComponents().get(0).setInput(true);

		List<VarInfo> group0 = new ArrayList<VarInfo>();
		List<VarInfo> group1 = new ArrayList<VarInfo>();
		
		// Group = 0, Rank = 0, B and C
		group0.add(new VarInfo(1, 0, model));
		group0.add(new VarInfo(2, 0, model));
		
		// Group = 0, Rank = 1, D and E
		group1.add(new VarInfo(3, 0, model));
		group1.add(new VarInfo(4, 0, model));
		
		
		Map<List<VarInfo>, LogicalModelUpdater> groupsRank0 = 
				new HashMap<List<VarInfo>, LogicalModelUpdater>();
		
		Map<List<VarInfo>, LogicalModelUpdater> groupsRank1 = 
				new HashMap<List<VarInfo>, LogicalModelUpdater>();

		groupsRank0.put(group0, UpdaterFactoryModelGrouping.getUpdater(model, "Synchronous"));
		groupsRank1.put(group1, UpdaterFactoryModelGrouping.getUpdater(model, "Synchronous"));		
		
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
		
		return mpc;
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
      Double[] rates = new Double[] {0.4,0.1,0.1,0.2,0.2};


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
  
	  @Test
      public void testPriotityRanks() throws IOException {
		  
		  ModelGrouping mpc = getMpcModel();
	   	  PriorityUpdater pc = new PriorityUpdater(mpc);
	   	  
	   	  byte[] initState = new byte[] {1,0,1,0,0};
	   	  
	      Map<String, Integer> finalStatesCount = new HashMap<String, Integer>();
	      
	      // cyclic attractor when Rank 0: B/C, Rank 1: D/E
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 0, 1, 0, 0}), 0);
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 1, 1, 0, 0}), 0);
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 1, 0, 0, 0}), 0);
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 0, 0, 0, 0}), 0);
		
	     int simRuns = 1000; 
 
	     for (int run = 0; run < simRuns ; run++) {
	    	 Random random = new Random();
		     int steps = random.nextInt(1000);
			 RandomWalkSimulation simulation = new RandomWalkSimulation(
					  new RandomUpdaterWrapper(pc),
		    		  initState,steps);
		    Iterator<byte[]> it = simulation.iterator();
		    byte[] successor = null;
	    	while (it.hasNext()) {
	            successor = it.next();
	    	}
            Integer count = finalStatesCount.get(Arrays.toString(successor));
            count ++;
            finalStatesCount.put(Arrays.toString(successor), count);                
	     }
	     
	     int size = finalStatesCount.keySet().size();
	     for (String key : finalStatesCount.keySet()) {
		     assertTrue(finalStatesCount.get(key) * 0.9 <= (1.0/size)*simRuns
		    		 && finalStatesCount.get(key) * 1.1 >= (1.0/size)*simRuns);
	     }
	     
	  }
	  
	  @Test
      public void testPCSim() throws IOException {
		  
		  ModelGrouping mpc = getMpcModel();
	   	  PriorityUpdater pc = new PriorityUpdater(mpc);
	   	  
	   	  byte[] initState = new byte[] {1, 0, 1, 1, 0};
	   	  
	      Map<String, Integer> finalStatesCount = new HashMap<String, Integer>();
	      
	      // cyclic attractor when Rank 0: B/C, Rank 1: D/E
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 0, 1, 1, 0}), 0);
	      // C update
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 1, 1, 1, 0}), 0);
	      // B update
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 1, 0, 1, 0}), 0);
	      // C update
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 0, 0, 1, 0}), 0);
	      
	      // in all these updates, E and D are also updatable
		
	     int simRuns = 1000; 
 
	     for (int run = 0; run < simRuns ; run++) {
	    	 Random random = new Random();
		     int steps = random.nextInt(1000);
			 RandomWalkSimulation simulation = new RandomWalkSimulation(
					  new RandomUpdaterWrapper(pc),
		    		  initState,steps);
		    Iterator<byte[]> it = simulation.iterator();
		    byte[] successor = null;
	    	while (it.hasNext()) {
	            successor = it.next();
	    	}
            Integer count = finalStatesCount.get(Arrays.toString(successor));
            count ++;
            finalStatesCount.put(Arrays.toString(successor), count);                
	     }
	     
	     int size = finalStatesCount.keySet().size();
	     for (String key : finalStatesCount.keySet()) {
		     assertTrue(finalStatesCount.get(key) * 0.9 <= (1.0/size)*simRuns
		    		 && finalStatesCount.get(key) * 1.1 >= (1.0/size)*simRuns);
	     }
	     
	  }
	  
	  
        
	  @Test
	  public void testRandomWalkAsyn() {
		LogicalModel model = getSimpleModel();
		  byte[] state = {1, 0, 0};
	      // C and D
	      Map<String, Integer> finalStatesCount = new HashMap<String, Integer>();
	      finalStatesCount.put(Arrays.toString(new byte[] {1, 1, 1}), 0);
	      finalStatesCount .put(Arrays.toString(new byte[] {0, 0, 0}), 0);

	      AsynchronousUpdater updater = new AsynchronousUpdater(model);
	      RandomWalkSimulation simulation = new RandomWalkSimulation(new RandomUpdaterWrapper(updater),
	    		  state,10);

	      Iterator<byte[]> it = simulation.iterator();

	     int simRuns = 1000; 
 
	     for (int run = 0; run < simRuns ; run++) {
	    	it = simulation.iterator();
		    byte[] successor = null;
	    	while (it.hasNext()) {
	            successor = it.next();
	    	}
            Integer count = finalStatesCount.get(Arrays.toString(successor));
            count ++;
            finalStatesCount.put(Arrays.toString(successor), count);                
	     }

	     assertTrue(finalStatesCount.get("[0, 0, 0]") * 0.9 <= 0.6875 * simRuns
	    		 && finalStatesCount.get("[0, 0, 0]") * 1.1 >= 0.6875 * simRuns);
	     
	     assertTrue(finalStatesCount.get("[1, 1, 1]") * 0.9 <= 0.3125 * simRuns
	    		 && finalStatesCount.get("[1, 1, 1]") * 1.1 >= 0.3125 * simRuns);
	     
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

