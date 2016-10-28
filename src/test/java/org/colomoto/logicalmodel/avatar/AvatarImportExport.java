package org.colomoto.logicalmodel.avatar;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.StatefulLogicalModel;
import org.colomoto.logicalmodel.StatefulLogicalModelImpl;
import org.colomoto.logicalmodel.io.avatar.AvatarExport;
import org.colomoto.logicalmodel.io.avatar.AvatarImport;
import org.colomoto.logicalmodel.io.avatar.AvatarUtils;
import org.colomoto.logicalmodel.tool.reduction.ModelReducer;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.Test;

public class AvatarImportExport extends TestCase {

	@Test
	public void testAvatarImport() {
		importAvatar(getAvatarInputFiles());
	}

	@Test
	public void testAvatarExport() {
		List<LogicalModel> models = importAvatar(getAvatarInputFiles());
		try {
			int k=0;
			for(LogicalModel model : models){
				/*byte[] state = null;
				if(model instanceof StatefulLogicalModel) state = ((StatefulLogicalModel)model).getInitialState();
				ModelReducer modelr = new ModelReducer(model);
				modelr.removePseudoOutputs();
				model = modelr.getModel(false);
				if(state != null) model = new StatefulLogicalModelImpl(model,state);*/
				AvatarExport export = new AvatarExport(model);
				File out = new File("testExport"+(k++)+".avatar");
				export.export(out);
			}
		} catch (Exception e) {
			e.printStackTrace();
			fail(e.getMessage());
		} 
	}

	private List<String> getAvatarInputFiles() {
		String dir = "C:\\Users\\Rui\\Documents\\00 Avatar\\Avatar Material\\table-models\\";
		return Arrays.asList(dir+"th-reduced.avatar");//"simple.avatar");
				/*"mmc-cycD1.avatar",dir+"mmc.avatar",
				dir+"random_001_v010_k2.avatar",dir+"random_002_v010_k2.avatar",
				dir+"random_003_v015_k2.avatar",dir+"random_004_v015_k2.avatar",
				dir+"sp1.avatar",dir+"sp2.avatar",dir+"sp4.avatar",
				dir+"synthetic_1.avatar",dir+"synthetic_2.avatar",dir+"th-reduced.avatar");*/
	}

	private List<LogicalModel> importAvatar(List<String> filenames) {
		List<LogicalModel> result = new ArrayList<LogicalModel>();
		for(String filename : filenames){
			System.out.println("FILE:"+filename);
			try {
				AvatarImport avatar = new AvatarImport(new File(filename));
				result.add(avatar.getModel());
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println(e);
				fail(e.getMessage());
			} 
		}
		return result;
	}
	
	@Test
	public void estSimpleModel() {
		List<LogicalModel> models = new ArrayList<LogicalModel>();
		models.add(simpleModel1());
		models.add(simpleModel2());
		models.add(simpleModel3());
		models.add(simpleModel4());
		models.add(simpleModel5());
		models.add(simpleModel6());
		int k=0;
		for(LogicalModel model : models){
			System.out.println(">>"+model.toString());
			try {
				AvatarExport export = new AvatarExport(model);
				File out = new File("testSimple"+k+".avatar");
				export.export(out);
			} catch (Exception e) {
				System.out.println(e);
				fail(e.getMessage());
			}			
		}
	}

	/** MODEL1 A => {0 => B, 1 => B, 2 => 0}, B => {0 => 1, 1 => 1, 2 => 0} **/ 
	public LogicalModel simpleModel1() {
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		for(int i=0; i<2; i++) variables.add(new NodeInfo("var"+i,(byte)2));
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo var : variables) mvf.add(var,(byte)(var.getMax()+1));
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf,10);
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] functions = new int[2];
		MDDVariable varA=vars[0], varB=vars[1];
		
		int[] childrenA=AvatarUtils.getFreeChildren(3), childrenB=AvatarUtils.getFreeChildren(3);
		childrenB[0]=1;
		childrenB[1]=1;
		childrenB[2]=0;
		int nodeB=varB.getNode(childrenB);
		
		childrenA[0]=nodeB;
		childrenA[1]=nodeB;
		childrenA[2]=0;
		int nodeA=varA.getNode(childrenA);

		functions[0]=nodeA;
		functions[1]=nodeB;
		return new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(AvatarUtils.getFreeState(2)), "model1");
	}
		
	/** MODEL2 newA => {0 => A, 1 => B, 2 => 1}, newB => {0 => B, 1 => A, 2 => 0} **/  
	public LogicalModel simpleModel2() {
		int nrVars = 4;
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		for(int i=0; i<nrVars; i++) variables.add(new NodeInfo("var"+i,(byte)2));
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo var : variables) mvf.add(var,(byte)(var.getMax()+1));
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf,10);
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] functions = new int[nrVars];
		MDDVariable A=vars[0], B=vars[1], newA=vars[2], newB=vars[3];
		
		int nodeA=A.getNode(AvatarUtils.getIdentityChildren(3));
		int nodeB=B.getNode(AvatarUtils.getIdentityChildren(3));

		int[] childrenA=AvatarUtils.getFreeChildren(3), childrenB=AvatarUtils.getFreeChildren(3);
		childrenA[0]=nodeA;	childrenA[1]=nodeB;	childrenA[2]=1;
		childrenB[0]=nodeB; childrenB[1]=nodeA; childrenB[2]=0;
		int nodeNewA=newA.getNode(childrenA);
		int nodeNewB=newB.getNode(childrenB);
		
		functions[0]=nodeA;
		functions[1]=nodeB;
		functions[2]=nodeNewA;
		functions[3]=nodeNewB;
		return new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(AvatarUtils.getFreeState(2)), "model2");
	}

	/** MODEL3 {A,B} newA {A=0&B=2:1 TRUE:0} newB {A=1&B=1:2 TRUE:0} **/  
	public LogicalModel simpleModel3() {
		int nrVars = 4;
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		for(int i=0; i<nrVars; i++) variables.add(new NodeInfo("var"+i,(byte)2));
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo var : variables) mvf.add(var,(byte)(var.getMax()+1));
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf,10);
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] functions = new int[nrVars];
		MDDVariable A=vars[0], B=vars[1], newA=vars[2], newB=vars[3];
		
		int nodeA=A.getNode(AvatarUtils.getIdentityChildren(3));
		int nodeB=B.getNode(AvatarUtils.getIdentityChildren(3));

		int newA1state = ddmanager.nodeFromState(new byte[]{0,2,-1,-1},1);
		int nodeNewA=newA.getNode(AvatarUtils.getChildrenWithSingleNode(3,newA1state));
		int newB1state = ddmanager.nodeFromState(new byte[]{1,1,-1,-1},2);
		int nodeNewB=newB.getNode(AvatarUtils.getChildrenWithSingleNode(3,newB1state));
		
		functions[0]=nodeA;
		functions[1]=nodeB;
		functions[2]=nodeNewA;
		functions[3]=nodeNewB;
		return new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(AvatarUtils.getFreeState(2)), "model3");
	}

	/** MODEL4 newA {A=0&B=2:1 A=1&B=2:1 A=0&B=1:2 TRUE:0} **/  
	public LogicalModel simpleModel4() {
		int nrVars = 3;
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		for(int i=0; i<nrVars; i++) variables.add(new NodeInfo("var"+i,(byte)2));
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo var : variables) mvf.add(var,(byte)(var.getMax()+1));
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf,10);
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] functions = new int[nrVars];
		MDDVariable A=vars[0], B=vars[1], newA=vars[2];
		
		int nodeA=A.getNode(AvatarUtils.getIdentityChildren(3));
		int nodeB=B.getNode(AvatarUtils.getIdentityChildren(3));

		int newAstate1 = ddmanager.nodeFromState(new byte[]{0,2,-1},1);
		int newAstate2 = ddmanager.nodeFromState(new byte[]{1,2,-1},1);
		int newAstate3 = ddmanager.nodeFromState(new byte[]{0,1,-1},2);
		int ORNode = MDDBaseOperators.OR.combine(ddmanager, new int[]{newAstate1,newAstate2,newAstate3});;
				
		int nodeNewA=newA.getNode(AvatarUtils.getChildrenWithSingleNode(3,ORNode));
		
		functions[0]=nodeA;
		functions[1]=nodeB;
		functions[2]=nodeNewA;
		return new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(AvatarUtils.getFreeState(2)), "model4");
	}


	/** MODEL5 B {B=0&A=2:1 B=1&A=2:1 B=0&A=1:2 TRUE:0} **/  
	public LogicalModel simpleModel5() {
		int nrVars = 2;
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		for(int i=0; i<nrVars; i++) variables.add(new NodeInfo("var"+i,(byte)2));
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo var : variables) mvf.add(var,(byte)(var.getMax()+1));
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf,10);
		MDDVariable[] vars = ddmanager.getAllVariables();
		int[] functions = new int[nrVars];
		MDDVariable A=vars[0], B=vars[1];
		int nodeA=A.getNode(AvatarUtils.getIdentityChildren(3));

		int nodeBState0 = MDDBaseOperators.OR.combine(ddmanager, 
				new int[]{
					ddmanager.nodeFromState(new byte[]{2,-1},1),
					ddmanager.nodeFromState(new byte[]{1,-1},2)});
		int nodeBState1 = ddmanager.nodeFromState(new byte[]{2,-1},1);
		int nodeBState2 = 0;
		int nodeB=B.getNode(new int[]{nodeBState0,nodeBState1,nodeBState2});
		
		functions[0]=nodeA;
		functions[1]=nodeB;
		return new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(AvatarUtils.getFreeState(2)), "model5");
	}
	
	/** MODEL6 A=>{A=0&B=2:1 TRUE:0} B=>{A=1&B=1:2 TRUE:0} **/  
	public LogicalModel simpleModel6() {
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
		return new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(AvatarUtils.getFreeState(2)), "model6");
	}
}
