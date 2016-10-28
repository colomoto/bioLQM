package org.colomoto.logicalmodel.io.avatar;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.StatefulLogicalModel;
import org.colomoto.logicalmodel.StatefulLogicalModelImpl;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.operators.MDDBaseOperators;

/**
 * Facilities to import a logical model from an Avatar document (specifying the variables, clauses, initializations and oracles)  
 * 
 * @author Rui Henriques, Pedro Monteiro
 * @version 1.0
 */
public class AvatarImport {

	private AvatarDocument document;
	private Map<String, Integer> identifier2index;
	private MDDManager ddmanager;
	private List<NodeInfo> variables; //ivariables;
	private byte[] initialState = null;
	private String name;
	public boolean quiet = true;

	/**
	 * Creates an Avatar Document from the given .AVATAR file
	 * @param f the .AVATAR file
	 * @throws IOException
	 */
	public AvatarImport(File f) throws IOException {
		name = f.getName();
		AvatarReader.checkFileSyntax(f);
		document = AvatarReader.loadAvatarDocumentFromFile(f);
		//System.out.println("Document:\n"+document.toString());
		getVariables();
		identifier2index = new HashMap<String, Integer>();
		int k=0;
		for(int l=variables.size(); k<l; k++) identifier2index.put(variables.get(k).getNodeID(),k);
		MDDVariableFactory mvf = new MDDVariableFactory();
		for(NodeInfo ni : variables) mvf.add(ni,(byte)(ni.getMax()+1));
		ddmanager = MDDManagerFactory.getManager(mvf,10);
	}
	
	/**
	 * Converts the internal Avatar Document into a stateful logical model
	 * @return logical model possibly defining a set of initial states and oracles
	 */
	public StatefulLogicalModel getModel() throws IOException {
		if(!quiet) System.out.println(identifier2index);
		MDDVariable[] ddvariables = ddmanager.getAllVariables();

		/** Builds MDD from DEFINE rules **/ 
		int[] functions = new int[variables.size()];
		for(String component : document.definitions.keySet()) {
			int idx = identifier2index.get(component);
			MDDVariable var = ddvariables[idx];

			List<AvatarClause> clauses = document.definitions.get(component);
			List<Integer> orNodes = new ArrayList<Integer>();
			//boolean last = false;
					
			boolean simplified = true;
			Map<Integer,List<byte[]>> pseudostatesPerState = new HashMap<Integer,List<byte[]>>();
			
			for(AvatarClause clause : clauses){
				if(!quiet) System.out.println(component+"("+idx+") | "+clause.toString());

				byte[] pseudostate = AvatarUtils.getFreeState(ddvariables.length);
				//boolean breaking = false;
				for(String comp : clause.getRestrictions().keySet()){
					if(comp.startsWith("DEFAULT")) break; 
						//breaking = true; }
					else pseudostate[identifier2index.get(comp)] = (byte)((int)clause.getRestrictions().get(comp));
				}
				//if(breaking) continue;
				if(simplified){
					int state = clause.getOutput();
					//System.out.println("::"+AvatarUtils.toString(pseudostate)+"=>"+state);
					if(!pseudostatesPerState.containsKey(state)) pseudostatesPerState.put(clause.getOutput(),new ArrayList<byte[]>());
					pseudostatesPerState.get(state).add(pseudostate);
				} else {
					int node = ddmanager.nodeFromState(pseudostate, clause.getOutput());
					int[] nodes = AvatarUtils.getFreeChildren(var.nbval);
					if(pseudostate[idx]>=0) nodes[pseudostate[idx]] = node;
					else nodes = AvatarUtils.getChildrenWithSingleNode(var.nbval, node);
					orNodes.add(var.getNode(nodes));
				}
			}
			//System.out.println("::"+pseudostatesPerState.toString());
			//check MDDBaseOperators.OR.combine since it generates rules that can be collapsed as one
			if(simplified){
				for(Integer state : pseudostatesPerState.keySet()) 
					orNodes.add(ddmanager.nodeFromStates(pseudostatesPerState.get(state), state));
				functions[idx] = MDDBaseOperators.OR.combine(ddmanager, AvatarUtils.toArray(orNodes));
			} else {
				functions[idx] = MDDBaseOperators.OR.combine(ddmanager, AvatarUtils.toArray(orNodes));
			}
				/* OLD CODE
				int lastNode=1;
				for(String comp : clause.getRestrictions().keySet()){
					if(comp.equals("DEFAULT")){
						last = true;
						break;
					}
					MDDVariable varLeaf = ddvariables[identifier2index.get(comp)];
					System.out.println("Comp("+identifier2index.get(comp)+"):"+comp+"["+varLeaf.nbval+"] lastNode:"+lastNode);
					if(varLeaf.nbval==2){
						if(clause.getOutput()==1) lastNode = varLeaf.getNode(0,lastNode);
						else lastNode = var.getNode(lastNode,0);
					} else {
						int[] children = AvatarUtils.getFreeChildren(varLeaf.nbval);
						children[clause.getRestrictions().get(comp)]=lastNode;
						lastNode = varLeaf.getNode(children);
					}
				}
				if(last) break;
				System.out.println("Node count:"+ddmanager.getNodeCount());

				if(var.nbval==2){
					if(clause.getOutput()==1) orNodes.add(var.getNode(0,lastNode));
					else orNodes.add(var.getNode(lastNode,0));
				} else {
					int[] rootChildren = AvatarUtils.getFreeChildren(var.nbval);
					rootChildren[clause.getOutput()]=lastNode; 
					orNodes.add(var.getNode(rootChildren));
				}*/
		} 
		
		/** NEW: Builds MDD from DEFINE rules **/
		/*int[] ifunctions = new int[ivariables.size()];
		for(NodeInfo ivar : ivariables){
			int idx = identifier2index.get(ivar.getNodeID());
			int[] nodes = AvatarUtils.getChildrenWithSingleNode(ivar.getMax()+1,idx);
			byte[] pseudostate = AvatarUtils.getFreeState(ddvariables.length);
			ifunctions[i] = ddmanager.nodeFromStates(nodes);
		}*/
		StatefulLogicalModel model = new StatefulLogicalModelImpl(variables, ddmanager, functions, Arrays.asList(getInitialState()), name);
		if(document.hasOracle()){
			List<List<byte[]>> oracles = new ArrayList<List<byte[]>>();
			List<byte[]> oracle = new ArrayList<byte[]>();
			for(Map<String,Integer> state : document.attractor){
				byte[] s = AvatarUtils.getFreeState(identifier2index.size());
				for(String n : state.keySet()) s[identifier2index.get(n)]=(byte)(int)state.get(n);
				oracle.add(s);
			}
			oracles.add(oracle);
			model.setOracles(oracles);
		}
		if(!quiet) System.out.println("Functions="+AvatarUtils.toString(functions)+"\n>>"+model.toString());
		return model;
	}

	private void getVariables() {
		variables = new ArrayList<NodeInfo>();
		for(String key : document.vars.keySet())
			variables.add(new NodeInfo(key,(byte)((int)document.vars.get(key)-1)));
		for(String key : document.ivars.keySet()){
			NodeInfo node = new NodeInfo(key,(byte)((int)document.ivars.get(key)-1));
			node.setInput(true);
			variables.add(node);
		}
	}

	private byte[] getInitialState() {
		if(initialState==null){
			initialState = AvatarUtils.getFreeState(variables.size());
			for(String component : document.inits.keySet())
				initialState[identifier2index.get(component)] = (byte)((int)document.inits.get(component));
		}
		return initialState;
	}
}
