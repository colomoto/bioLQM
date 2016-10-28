package org.colomoto.logicalmodel.io.avatar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Class defining an Avatar Document (to help with import/export facilities)
 *  
 * @author Rui Henriques, Pedro Monteiro
 */
public class AvatarDocument {

	/** input variables */
	public Map<String,Integer> ivars;
	/** non-input variables */
	public Map<String,Integer> vars;
	/** clauses */
	public Map<String,List<AvatarClause>> definitions;
	/** variable initializations */
	public Map<String,Integer> inits; //coreRestrictions
	/** oracles */
	public List<Map<String,Integer>> attractor;
	
	/**
	 * Creates an empty Avatar Document 
	 */
	public AvatarDocument(){
		ivars = new HashMap<String,Integer>();
		vars = new HashMap<String,Integer>();
		definitions = new HashMap<String,List<AvatarClause>>();
		inits = new HashMap<String,Integer>(); //coreRestrictions
		attractor = new ArrayList<Map<String,Integer>>();
	}
	
	/**
	 * Initializes a component
	 * @param component the variable to be initialized
	 * @param init the initial state of the variable
	 */
	public void addInitRestriction(String component, int init){
		inits.put(component,init);
	}
	
	/**
	 * Adds a set of clauses defining the changes to the state of a given component
	 * @param component the target variable
	 * @param clauses the list of clauses regulating its state
	 */
	public void addDefinition(String component, List<AvatarClause> clauses){
		definitions.put(component,clauses);
	}
	
	/**
	 * Adds an input variable/component to the document
	 * @param component the input variable to be added
	 * @param nrStates the number of states of the variable
	 */
	public void addIVar(String component, int nrStates){
		ivars.put(component, nrStates);
	}
	
	/**
	 * Adds a non-input variable/component to the document
	 * @param component the variable to be added
	 * @param nrStates the number of states of the variable
	 */
	public void addVar(String component, int nrStates){
		vars.put(component, nrStates);
	}
	
	/**
	 * Checks whether the document has a given component
	 * @param component the variable ID
	 * @return true if the variable is in the document
	 */
	public boolean hasComponent(String component) {
		return ivars.containsKey(component)||vars.containsKey(component);
	}
	
	/**
	 * Checks whether the document has oracles
	 * @return true if the document has one or more oracles
	 */
	public boolean hasOracle() {
		return attractor!=null && attractor.size()>0;
	}
	
	/**
	 * Returns the number of states of a given component
	 * @param component the ID of the variable to be checked
	 * @return the number of states from the given variable
	 */
	public int getNumberStates(String component) {
		if(vars.containsKey(component)) return vars.get(component);
		else return ivars.get(component);
	}
	
	/**
	 * Adds an oracle to the document
	 * @param oracle list of state-patterns defining an oracle
	 */
	public void addOraclePart(Map<String,Integer> oracle) {
		attractor.add(oracle);
	}
	
	@Override
	public String toString(){
		return"Vars: "+vars+"\nDefinitions: "+definitions+"\nInits: "+inits+"\nOrcale: "+attractor;
	}
}
