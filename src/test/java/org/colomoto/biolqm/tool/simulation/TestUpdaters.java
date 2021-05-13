package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.BlockSequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.grouping.SplittingType;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping.VarInfo;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.PriorityUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomAsynchUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;
import org.colomoto.biolqm.widgets.UpdaterFactoryModelGrouping;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.jupiter.api.Test;

import junit.framework.Assert;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
	
	private LogicalModel getThirdModel() {
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

		functions[0] = 1;
		functions[1] = 1;
		functions[2] = 1;
		functions[3] = 1;
		functions[4] = 1;
		
		return new LogicalModelImpl(vars, manager, functions);
	}
	

	private ModelGrouping getMpcModel() throws IOException {
		// LogicalModel, Map<Rank, Map<List<GroupVars>,updater>>
		
		LogicalModel model = getOtherModel();
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
	
	//private int findChange(byte[] state1, byte[] state2) {
	//}
	
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
	public void testRandomAsynchronousUpdater() throws IOException {
		LogicalModel model = getModel();
		RandomUpdater updater = new RandomAsynchUpdater(model);
		byte[] state = {0,0,0};
		byte[] successor = updater.pickSuccessor(state);


	}
	
	@Test
	public void testRandomAsynchronousUpdaterOther() throws IOException {
		LogicalModel model = getOtherModel();
		RandomUpdater updater = new RandomAsynchUpdater(model);
		byte[] state = {0,0,0,0,0};
		byte[] successor = updater.pickSuccessor(state);

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
		String scheme = "A,C,D:B,E";
		ModelGrouping grouping = new ModelGrouping(model, scheme);
        DeterministicUpdater updater = new BlockSequentialUpdater(grouping);
		byte[] state = {1,1,1,1,1};
        byte[] next = updater.getSuccessor(state);
		
		assertEquals(1, next[0]);
		assertEquals(0, next[1]);
		assertEquals(0, next[2]);
		assertEquals(0, next[3]);
		assertEquals(1, next[4]);

		String scheme1 = "A,B,C,D:E";
		grouping = new ModelGrouping(model, scheme1);
		updater = new BlockSequentialUpdater(grouping);
        next = updater.getSuccessor(state);
		
		assertEquals(1, next[0]);
		assertEquals(1, next[1]);
		assertEquals(0, next[2]);
		assertEquals(0, next[3]);
		assertEquals(1, next[4]);

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
		model.getComponents().get(1).setInput(true);
        DeterministicUpdater updater = new SequentialUpdater(model);
		byte[] state = {0,0,0};
        byte[] next = updater.getSuccessor(state);
		
		assertEquals(1, next[0]);
		assertEquals(1, next[1]);
		assertEquals(1, next[2]);
	}

	@Test
	public void testCustomSequentialUpdater() {
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
	public void testPriorityUpdater() {
		LogicalModel model = getOtherModel();
		// One class Sync
		ModelGrouping mpc = new ModelGrouping(model,
				"A" + ModelGrouping.SEPVAR +
				"B" + ModelGrouping.SEPVAR +
				"C" + ModelGrouping.SEPVAR +
				"D" + ModelGrouping.SEPVAR +
				"E");
		
		PriorityUpdater updater = new PriorityUpdater(mpc);
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
		updater = new PriorityUpdater(mpc);
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
		updater = new PriorityUpdater(mpc);
		lNext = updater.getSuccessors(state);
		assertEquals(1, lNext.size());
		assertEquals(1, lNext.get(0)[0]);
		assertEquals(0, lNext.get(0)[1]);
		assertEquals(0, lNext.get(0)[2]);
		assertEquals(1, lNext.get(0)[3]);
		assertEquals(1, lNext.get(0)[4]);
	}
	
	@Test
	public void testRandomUpdaterWithRatesBasic() throws IOException {
		LogicalModel model = getModel();
		Double[] rates =  new Double[] {0.5,0.3,0.2};
		Map<String, Double> nodeRates = new HashMap<String, Double>();
		for (int i = 0; i < model.getComponents().size(); i++) {
			nodeRates.put(model.getComponents().get(i).getNodeID(), rates[i]);
		}
			
		ModelGrouping mpc = new ModelGrouping(model);
		mpc.addUpdater(0, 0, nodeRates);
		
		PriorityUpdater updater = new PriorityUpdater(mpc);
		
		byte[] state = {0,0,0};
		// two updatable states A and B
		// 0.5/0.8 = 0.625
		// 0.3/0.8 = 0.375
		
		byte[] state2 = {1,1,0};
		// one updatable state, C
		// 0.3/0.3
		
		state = state2.clone();
		
		int size = model.getComponents().size();

		Set<Integer> updatables = new HashSet<Integer>();		
		int[] simUpdates = new int[size];
		int simRuns = 10000;
		
		int run = 0;
		for (; run < simRuns ; run++) {
			List<byte[]> successor = updater.getSuccessors(state);
			// System.out.println(Arrays.toString(successor));
			if (successor == null) {
				break;
			}
			assertTrue(successor.size() == 1);
			int idx = getIdxChange(state, successor.get(0));
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
	public void testRandomUpWithRatesFilter() throws IOException {
		LogicalModel model = getModel();
		Map<NodeInfo, SplittingType> filter = new HashMap<NodeInfo, SplittingType>();
		SplittingType[] test = new SplittingType[]{SplittingType.MERGED, SplittingType.NEGATIVE, SplittingType.POSITIVE};
		
		int ratesCount = 0;

        Random random = new Random();
		for (NodeInfo node : model.getComponents()) {
			SplittingType splt = test[random.nextInt(test.length)];
			if (splt != null)
				ratesCount +=2;
			filter.put(node, splt);
		}
	
		RandomUpdaterWithRates updater = new RandomUpdaterWithRates(model);
		assert(updater.getRates().length == ratesCount);
	}
	
	
	@Test
	public void testRandomUpdaterWithRatesAndSplit() throws IOException {
				
		LogicalModel model = getOtherModel();
		
		// C has different rates for [-] 0.7 and [+] 0.2
		Double[] rates =  new Double[] {0.5,0.5, 0.3,0.3, 0.7,0.2, 0.1,0.1, 0.4,0.4};
		Map<NodeInfo, SplittingType> filter = new HashMap<NodeInfo, SplittingType>();
		
		
		// split transitions 
		SplittingType[] test = new SplittingType[]{SplittingType.MERGED, SplittingType.MERGED,
				SplittingType.MERGED, SplittingType.POSITIVE, SplittingType.POSITIVE};
		
		int i = 0;
		for (NodeInfo node : model.getComponents()) {
			SplittingType splt = test[i];
			filter.put(node, splt);
			i++;
		}
		RandomUpdaterWithRates updater = new RandomUpdaterWithRates(model, rates);
		updater.setFilter(filter);
	
		// updatables = C[+], D[-] and E[+].
		byte[] state = {1,0,0,1,0};
		// 0.2, 0.1, 0.4
		// 0.28, 0.14, 0.57
		// with split transitions : 0.2, 0.4 > 0.3, 0.6

		// updatables = C[-], and E[+].
		byte[] state2 = {1,1,1,0,0};
		// 0.7, 0.4
		// 0.63, 0.36
		// with split transitions : ... same 
		state = state2.clone();
		
		int size = model.getComponents().size();

		Set<Integer> updatables = new HashSet<Integer>();	
		// for both [-] and [+]
		int[] simUpdates = new int[size * 2];
		int simRuns = 1000;
		
		for (int run = 0; run < simRuns ; run++) {
			byte[] successor = updater.pickSuccessor(state);
			if (successor == null) {
				break;
			}
			
			int idx = getIdxChange(state, successor);
			int change = getChange(state, successor);
			
			if (change == 1) {
				simUpdates[idx * 2 + 1] += 1;
				updatables.add(idx * 2 + 1);

			} else {
				simUpdates[idx * 2] += 1;
				updatables.add(idx * 2);
			}	
		}
				
		double[] newRates = new double[size * 2];
		double sum = 0.0;
		// sum of rates of updatable components 
		for (int id : updatables) {
			sum += rates[id];
			}
		
		// calculate new rates
		for (int id : updatables) {
			newRates[id] = rates[id]/sum;
		}

		for (int compIdx = 0; compIdx < size * 2; compIdx++) {
			assertTrue(simUpdates[compIdx] >= simRuns * newRates[compIdx] * 0.9 
					&& simUpdates[compIdx] <= simRuns * newRates[compIdx] * 1.1);
		}
		
	}
	
	@Test
	public void testRandomUpdaterRateFilter() throws IOException {
		
	LogicalModel model = getOtherModel();
		
		Map<NodeInfo, SplittingType> filter = new HashMap<NodeInfo, SplittingType>();
		
		// split transitions 
		SplittingType[] test = new SplittingType[]{SplittingType.MERGED, SplittingType.NEGATIVE,
				SplittingType.POSITIVE};
		
		Random random = new Random();
		for (int j = 0; j <  random.nextInt(model.getComponents().size()); j++) {
			SplittingType splt = test[random.nextInt(test.length)];
			filter.put(model.getComponents().get(j), splt);
		}
		RandomUpdaterWithRates updater = new RandomUpdaterWithRates(model);
		assert(updater.getRates().length == model.getComponents().size()*2);
	}
	
	
	@Test
	public void testRandomUpdaterWithRates() throws IOException {
		
		LogicalModel model = getOtherModel();
		Double[] rates = {0.4,0.1,0.1,0.2,0.2};
		
		RandomUpdater updater = new RandomUpdaterWithRates(model, rates);
		byte[] state = {0,0,0,0,0};
		// two updatable states C and D
		// [0, 0, 0.33, 0.66, 0]
	
		
		byte[] state2 = {0,1,0,0,0};
		// three updatable states, B, D and E
		// [0, 0.2, 0, 0.4, 0.4]
		
		state = state2.clone();
		
		int size = model.getComponents().size();
		int[] simUpdates = new int[size];
		Set<Integer> updatables = new HashSet<Integer>();
		int simRuns = 10000;
		
		for (int run = 0; run < simRuns ; run++) {
			byte[] successor = updater.pickSuccessor(state);
			if (successor == null) {
				break;
			}
			int idx = getIdxChange(state, successor);
			simUpdates[idx] += 1;
			// save idx of component updated 
			updatables.add(idx);
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
	public void testRandomUpdaterWithRatesOtherInput() throws IOException {
		
		LogicalModel model = getOtherModel();
		model.getComponents().get(1).setInput(true);
		Double[] rates = {0.4,0.1,0.1,0.2,0.2};
		
		RandomUpdater updater = new RandomUpdaterWithRates(model, rates);
		byte[] state = {0,0,0,0,0};
		// two updatable states C and D
		// [0, 0, 0.33, 0.66, 0]
	
		
		byte[] state2 = {0,1,0,0,0};
		// three updatable states, B, D and E
		// [0, 0.2, 0, 0.4, 0.4]
		
		state = state2.clone();
		
		int size = model.getComponents().size();
		int[] simUpdates = new int[size];
		Set<Integer> updatables = new HashSet<Integer>();
		int simRuns = 10000;
		
		for (int run = 0; run < simRuns ; run++) {
			byte[] successor = updater.pickSuccessor(state);
			if (successor == null) {
				break;
			}
			int idx = getIdxChange(state, successor);
			simUpdates[idx] += 1;
			// save idx of component updated 
			updatables.add(idx);
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
	
	
	
	//@Test
	public void testRandomUpdaterWraper() throws IOException {
		LogicalModel model = getOtherModel();
		
		MultipleSuccessorsUpdater MultiUpdater = new AsynchronousUpdater(model);

		RandomUpdater updater = new RandomUpdaterWrapper(MultiUpdater);
		byte[] state = {0,0,0,0,0};
		
		// two updatable states C and D
	    Set<Integer> updatables = new HashSet<Integer>();
		int size = model.getComponents().size();
		int[] simUpdates = new int[size];
		int simRuns = 10000;
		
		
		for (int run = 0; run < simRuns ; run++) {
			byte[] successor = updater.pickSuccessor(state);
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
//	
//		@Test
//		public void testPriorityClassesModelGrouping() throws IOException {
//			LogicalModel model = getThirdModel();
//			List<NodeInfo> nodes = model.getComponents();
//			model.getComponents().get(0).setInput(true);
//			int nodeCount = 0;
//			String modelGroup = "";
//			for (NodeInfo node : nodes) {
//				if (!node.isInput()) {
//					modelGroup += node.toString() + ModelGrouping.SEPGROUP;
//					nodeCount ++;
//				}
//			}
//			modelGroup = modelGroup.substring(0, modelGroup.length() - 
//					ModelGrouping.SEPGROUP.length());
//			
//			ModelGrouping mpc = new ModelGrouping(model, modelGroup);
//			
//			PriorityUpdater pc = new PriorityUpdater(mpc);
//			byte[] state = {0,0,0,0,0};
//
//			List<byte[]> lNext = pc.getSuccessors(state);
//			assertEquals(nodeCount, lNext.size());
//
//		}
		
		@Test
		public void testPCsMpcStateChange() throws IOException {
			LogicalModel model = getThirdModel();
			List<NodeInfo> nodes = model.getComponents();
			int nodeCount = 0;
			String modelGroup = "";
			for (NodeInfo node : nodes) {
				if (!node.isInput()) {
					modelGroup += node.toString() + ModelGrouping.SEPGROUP;
					nodeCount ++;
				}
			}
			modelGroup = modelGroup.substring(0, modelGroup.length() - 
					ModelGrouping.SEPGROUP.length());
			
			ModelGrouping mpc = new ModelGrouping(model, modelGroup);
			
			PriorityUpdater pc = new PriorityUpdater(mpc);
			byte[] state = {0,0,0,0,0};

			List<byte[]> lNext = pc.getSuccessors(state);
			assertEquals(nodeCount, lNext.size());
			
			int i = 0;
			for (byte[] currState : lNext) {
				assertEquals(1, getNumChanges(state, currState));
				assertEquals(i, getIdxChange(state, currState));
				i++;
			}

		}
		
		@Test
		public void testPCGroupSucc() throws IOException {
			LogicalModel model = getThirdModel();
			ModelGrouping mpc = new ModelGrouping(model, "A" + ModelGrouping.SEPVAR +
														 "B" + ModelGrouping.SEPVAR +
														 "C" + ModelGrouping.SEPUPDATER +
														 "RN[2.0,2.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0]" + 
														 ModelGrouping.SEPGROUP +
														 "D" + ModelGrouping.SEPVAR +
														 "E");
						
			PriorityUpdater pc = new PriorityUpdater(mpc);
			byte[] state = {0,0,0,0,0};
			// A,B,C w/ prob 2.0, 0 and 1.0 respc.
			// D,E are synchronous
			byte[] firstCG = new byte[] {0,0,1,0,0};
			byte[] firstAG = new byte[] {1,0,0,0,0};
			
			byte[] secondG =  new byte[] {0,0,0,1,1};
			
			for (int i = 0; i < 1000; i++) {
				List<byte[]> lNext = pc.getSuccessors(state);
				Assert.assertTrue(lNext.size() == 2);
				Assert.assertTrue((Arrays.equals(lNext.get(0),firstAG)) ||
						Arrays.equals(lNext.get(0),firstCG));
				Assert.assertTrue(Arrays.equals(lNext.get(1), secondG));
			}
			
		}
		
	//@Test
	public void testPCGroupChooice() throws IOException {
		ModelGrouping mpc = getMpcModel();
		PriorityUpdater pc = new PriorityUpdater(mpc);		

		byte[] state = new byte[] {1,1,1,0,0};
		byte[] cChange = new byte[] {1,1,0,0,0};
		byte[] eChange = new byte[] {1,1,1,0,1};

		List<byte[]> lNext = pc.getSuccessors(state);
		Assert.assertTrue(Arrays.equals(lNext.get(0),cChange));
		
		mpc.switchClasses(0, 1);
		List<byte[]> lNewNext = pc.getSuccessors(state);
		Assert.assertTrue(Arrays.equals(lNewNext.get(0),eChange));
		
		lNewNext = pc.getSuccessors(state);
		Assert.assertTrue(Arrays.equals(lNewNext.get(0),eChange));
			
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
	
	private int getChange(byte[] state1, byte[] state2) {
		int idx = 0;
		boolean foundchange = false;
		while (!foundchange && idx < state1.length) {
			if (state2[idx] != state1[idx]) {
				return state2[idx] - state1[idx];
			}
			else {
				idx += 1; 
			}			
		}
		return 0;
	}
	
	private int getNumChanges(byte[] state1, byte[] state2) {
		int idx = 0;
		int change = 0;
		while (idx < state1.length) {
			if (state2[idx] != state1[idx]) {
				change ++;
			}
			idx += 1; 
		}
		return change;
	}
	
}















