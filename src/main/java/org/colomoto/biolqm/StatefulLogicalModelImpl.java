package org.colomoto.biolqm;

import java.util.List;

import org.colomoto.mddlib.MDDManager;

/**
 * Extension of a logical model with the possibility to maintain a set of initial states and oracles.<br> 
 * Implements the StatefulLogicalModel interface.
 * 
 * @author Rui Henriques
 */
public class StatefulLogicalModelImpl extends LogicalModelImpl implements StatefulLogicalModel {

	protected List<byte[]> initialStates;
	protected List<List<byte[]>> oracles;
	protected String name;
	
	/**
	 * Creates a stateful logical model based on a MDD
	 * @param ddmanager the MDD manager
	 * @param coreNodes the MDD core components
	 * @param coreFunctions the MDD core nodes
	 * @param extraNodes the MDD extra components
	 * @param extraFunctions the MDD extra nodes
	 * @param states the set of initial states
	 * @param _name the name of the logical model
	 */
	public StatefulLogicalModelImpl(MDDManager ddmanager, List<NodeInfo> coreNodes, int[] coreFunctions, List<NodeInfo> extraNodes, int[] extraFunctions, List<byte[]> states, String _name) {
		super(ddmanager,coreNodes,coreFunctions,extraNodes,extraFunctions);
		initialStates = states;
		name = _name;
	}
	
	/**
	 * Creates a stateful logical model based on a MDD
	 * @param nodeOrder the MDD components
	 * @param ddmanager the MDD manager
	 * @param functions the MDD nodes
	 * @param states the set of initial states
	 * @param _name the name of the logical model
	 */
	public StatefulLogicalModelImpl(List<NodeInfo> nodeOrder, MDDManager ddmanager, int[] functions, List<byte[]> states, String _name) {
		super(ddmanager, nodeOrder, functions, null, null);
		initialStates = states;
		name = _name;
	}
	
	/**
	 * Creates a stateful logical model based on an existing logical model
	 * @param model the logical model to clone
	 * @param states the set of initial states to be added
	 */
	public StatefulLogicalModelImpl(LogicalModel model, List<byte[]> states) {
		this(model.getMDDManager(), model.getComponents(), model.getLogicalFunctions(),model.getExtraComponents(),model.getExtraLogicalFunctions(),states,"");
	}
	
	@Override
	public String getName() {
		if(name==null) name = "undefined-name";
		return name;
	}
	
	@Override
	public List<byte[]> getInitialStates() {
		return initialStates;
	}
	
	@Override
	public List<List<byte[]>> getOracles() {
		return oracles;
	}
	
	@Override
	public void setInitialStates(List<byte[]> states) {
		initialStates = states;
	}
	
	@Override
	public void setOracles(List<List<byte[]>> oracle) {
		oracles = oracle;
	}
	
	@Override
    public String toString(){ 
    	String result = super.toString()+"\nInitial States:\n";
    	for(byte[] initialState : initialStates){
    		for(int i=0, l=initialState.length; i<l; i++) 
    			result+=initialState[i]+",";
    		result+="\n";
    	}
    	return result;
    }
}
