package org.colomoto.biolqm.io.avatar;

import java.util.HashMap;
import java.util.Map;

/**
 * Defines an Avatar clause (how the state of a component changes as a function of its regulators) 
 * @author Rui Henriques, Pedro Monteiro
 * @version 1.0
 */
public class AvatarClause {

	private Integer output = -1;
	private Map<String,Integer> restrictions = new HashMap<String,Integer>();
	
	/**
	 * Adds a restriction
	 * @param component the ID of the component
	 * @param state the state of the component
	 */
	public void addRestriction(String component, int state){
		restrictions.put(component,state); 
	}
	
	/**
	 * Adds the output value given the inputted restrictions
	 * @param _output the target state of the current component
	 */
	public void addOutput(int _output){
		output = _output;
	}
	
	/**
	 * Returns the target state of the regulated component
	 * @return the state of the current component
	 */
	public Integer getOutput(){
		return output;
	}
	
	/**
	 * Returns the list of restrictions imposing a well-defined state in the current component
	 * @return the list of restrictions 
	 */
	public Map<String,Integer> getRestrictions(){
		return restrictions;
	}
	
	@Override
	public String toString(){
		return output+" <= "+restrictions.toString();
	}
}
