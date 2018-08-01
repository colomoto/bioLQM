package org.colomoto.biolqm.tool.simulation.avatar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.StatefulLogicalModelImpl;
import org.colomoto.biolqm.io.avatar.AvatarUtils;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.junit.jupiter.api.Test;

public class AvatarSimulationTests {

	@Test
	public void testFileSimulations() {
		List<LogicalModel> models = new ArrayList<LogicalModel>();
		models.add(getModerateModel());
		models.add(getSimpleModel());
		for(LogicalModel model : models){
			byte[] istate = ((StatefulLogicalModelImpl)model).getInitialStates().get(0);
			System.out.println("Stateful model: "+model.toString());
			
			FirefrontSimulation fSimulation = new FirefrontSimulation(model,istate);
			fSimulation.runSimulation();
			
			AvatarSimulation avaSimulation = new AvatarSimulation(model,istate);
			avaSimulation.runSimulation();
		}
	}

	public void estModelSimulation() {
		List<LogicalModel> models = new ArrayList<LogicalModel>();
		models.add(getSimpleModel());
		models.add(getModerateModel());
		for(LogicalModel model : models){
			byte[] istate = ((StatefulLogicalModelImpl)model).getInitialStates().get(0);
			System.out.println("\n\nStateful model: "+model.toString());
			
			FirefrontSimulation simulation = new FirefrontSimulation(model,istate);
			simulation.runSimulation();
		}
	}

	/** MODEL
	 *  Rules: A=>{A=0&B=2:1 TRUE:0} B=>{A=1&B=1:2 TRUE:0} 
	 *  Initial state: {A=0, B=2, C=1} 
	 **/  
	private LogicalModel getModerateModel() {
		int nrVars = 3;
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		for(int i=0; i<nrVars; i++) variables.add(new NodeInfo("var"+i,(byte)2));
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo var : variables) mvf.add(var,(byte)(var.getMax()+1));
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf,10);
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] functions = new int[nrVars];
		MDDVariable A=vars[0], B=vars[1], C=vars[2];
		
		int nodeAState0 = ddmanager.nodeFromState(new byte[]{-1,2,-1},1);
		int nodeAState1 = 0;
		int nodeAState2 = 0;
		int nodeA=A.getNode(new int[]{nodeAState0,nodeAState1,nodeAState2});

		int nodeBState0 = 0;
		int nodeBState1 = ddmanager.nodeFromState(new byte[]{1,-1,-1},2);
		int nodeBState2 = 0;
		int nodeB=B.getNode(new int[]{nodeBState0,nodeBState1,nodeBState2});
		
		int nodeC=C.getNode(AvatarUtils.getFreeChildren(C.nbval));
		
		functions[0]=nodeA;
		functions[1]=nodeB;
		functions[2]=nodeC;
		
		byte[] initialState = new byte[]{0,2,0};
		LogicalModel model = new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(initialState), "moderateModel");
		
		return model;
	}

	/** MODEL
	 *  Rules: A=>{A=0&B=2:1 TRUE:0} B=>{A=1&B=1:2 TRUE:0} 
	 *  Initial state: {A=0, B=2} 
	 **/  
	public LogicalModel getSimpleModel() {
		int nrVars = 2;
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		for(int i=0; i<nrVars; i++) variables.add(new NodeInfo("var"+i,(byte)2));
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo var : variables) mvf.add(var,(byte)(var.getMax()+1));
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf,10);
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] functions = new int[nrVars];
		MDDVariable A=vars[0], B=vars[1];
		
		int nodeAState0 = ddmanager.nodeFromState(new byte[]{-1,2},1);
		int nodeAState1 = 0;
		int nodeAState2 = 0;
		int nodeA=A.getNode(new int[]{nodeAState0,nodeAState1,nodeAState2});

		int nodeBState0 = 0;
		int nodeBState1 = ddmanager.nodeFromState(new byte[]{1,-1},2);
		int nodeBState2 = 0;
		int nodeB=B.getNode(new int[]{nodeBState0,nodeBState1,nodeBState2});
		
		functions[0]=nodeA;
		functions[1]=nodeB;
		
		byte[] initialState = new byte[]{0,2};
		return new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(initialState), "simpleModel");
	}

	/*@Test
	public void testAvatarSimulation() {
		String fpath = "C:\\Users\\Rui\\Documents\\00 PosDoc\\Avatar Material\\table-models\\";
		String filename = fpath + "mmc-cycD1.avatar";
		try {
			AvatarImport avatar = new AvatarImport(new File(filename));
			LogicalModel model = avatar.getModel();
			byte[] istate = avatar.getInitialState();
			System.out.println("Initial state: "+AvatarUtils.toString(istate));
			
			AvatarSimulation simulation = new AvatarSimulation(model,istate);
			//simulation.addState(state);
			Iterator<byte[]> states = simulation.iterator();
			while(states.hasNext()){
				byte[] state = states.next();
				System.out.println(AvatarUtils.toString(state));
			}
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println(e);
			fail(e.getMessage());
		} 
	}

	@Test
	public void testFirefrontSimulation() {
		String fpath = "C:\\Users\\Rui\\Documents\\00 PosDoc\\Avatar Material\\table-models\\";
		String filename = fpath + "mmc-cycD1.avatar";
		try {
			AvatarImport avatar = new AvatarImport(new File(filename));
			LogicalModel model = avatar.getModel();
			byte[] istate = avatar.getInitialState();
			System.out.println("Initial state: "+AvatarUtils.toString(istate));
			
			FirefrontSimulation simulation = new FirefrontSimulation(model,istate);
			simulation.runSimulation();
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println(e);
			fail(e.getMessage());
		} 
	}*/

}
