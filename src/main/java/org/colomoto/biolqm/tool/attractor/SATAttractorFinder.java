package org.colomoto.biolqm.tool.attractor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolTask;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;
import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

/**
 * Identification of synchronous attractors using a SAT solver,
 * as proposed by E. Dubrova and M. Teslenko in 
 * "A SAT-Based Algorithm for Finding Attractors in Synchronous Boolean Networks"
 *
 * @author Mitchell Markin
 */
public class SATAttractorFinder extends AbstractToolTask<Object> {

	private Map<Integer, ArrayList<ArrayList<Integer>>> dnfFunctions;
	private Map<Integer, ArrayList<ArrayList<Integer>>> cnfFunctions;
	private Map<Integer, Integer> constants;
	
	//from the sat4j documentation, not sure if necessary
	private final int MAXVAR = 1000000;

	public SATAttractorFinder(LogicalModel model) {
		super(model);
		dnfFunctions = new HashMap<Integer, ArrayList<ArrayList<Integer>>>();
		cnfFunctions = new HashMap<Integer, ArrayList<ArrayList<Integer>>>();
		constants = new HashMap<Integer, Integer>();
	}

	public Object performTask() {
		MDDManager manager = model.getMDDManager();
	
		//convert the model into a set of functions in disjunctive normal form
		parseModel(model);

		//process DNF lists to remove constant values, sat4j does not have a way to input constants as far as I can tell
		processConstants();
		
		//strange corner case where the entire system is constant
		//just print the system as is and return, otherwise Sat4j will start throwing ContradictionExceptions
		if(dnfFunctions.size() == 0){
			System.out.println("Found Constant System:");
			System.out.print("\t");
			boolean first = true;
			for(Map.Entry<Integer, Integer> mapping : constants.entrySet()){
				if(first){
					first = false;
				} else {
					System.out.print(" & ");
				}
			
				MDDVariable var = manager.getAllVariables()[mapping.getKey()-1];
			
				if(mapping.getValue() == 0){
					System.out.print("!");
				} 
				System.out.print(var.toString());
			}

			// FIXME: return a result object
			return null;
		}

		//create the conjunctive normal form representation of each function
		disjunctiveToConjunctive();

		//The following algorithm was presented by Elana Dubrova and Maxim Teslenko in 
		//"A SAT-Based Algorithm for Finding Attractors in Synchronous Boolean Networks"
		
		//i, aka. the number of time steps the algorithm compounds into a single boolean expression at once,
		//can cause the heap to overflow as it doubles in size every time the algorithm fails to find an attractor
		//if desired, i can be limited to prevent this
		int i = (dnfFunctions.size() + constants.size());
		if(i > 100){
			i = 100;
		}
		boolean attractorIsFound = false;
		
		ArrayList<ArrayList<ArrayList<Integer>>> allAttractors = new ArrayList<ArrayList<ArrayList<Integer>>>();
		ArrayList<ArrayList<Integer>> problem = new ArrayList<ArrayList<Integer>>();
		
		for(int j = 0; j < i; j++){
			writeTimeStep( problem, j);
		}

		try{
			ISolver solver = SolverFactory.newDefault();

			solver.newVar(MAXVAR);
			//the timeout is for a single attempt to satisfy the expression in problem,
			//it is not a timeout for the algorithm as a whole
			solver.setTimeout(30);

			for(int k = 0; k < problem.size(); k++){
				int[] clause = new int[problem.get(k).size()];
				for(int j = 0; j < problem.get(k).size(); j++){
					clause[j] = problem.get(k).get(j);
				}
				solver.addClause(new VecInt(clause));
			}

			while(solver.isSatisfiable()){
				int[] solutionModel = solver.model();

				int numFunctions = dnfFunctions.size();
				int offset = dnfFunctions.size() + constants.size();

				int[][] path = new int[i+1][numFunctions];

				//ASSUMPTION: solutionModel is sorted when it is returned, such that variable 1 is first whether it is positive or negative
				for(int k = 0; k < solutionModel.length; k++){
					int temp = solutionModel[k];
					if(temp < 0){
						temp *= -1;
						temp = ((temp - 1) % offset) + 1;
						temp *= -1;
					} else {
						temp = ((temp - 1) % offset) + 1;
					}
					path[k/numFunctions][k%numFunctions] = temp;
				}

				for(int k = 1; k < path.length; k++){
					boolean match = true;
					for (int j = 0; j < path[0].length; j++){
						if(path[0][j] != path[k][j]){
							match = false;
						}
					}
					
					if(match){
						attractorIsFound = true;
						ArrayList<ArrayList<Integer>> attractor = new ArrayList<ArrayList<Integer>>(); 
						for(int j = 0; j < k; j++){
							ArrayList<Integer> record = new ArrayList<Integer>();
							int[] newClause = new int[path[j].length];
							
							for(int l = 0; l < newClause.length; l++){
								record.add(path[j][l]);
								newClause[l] = -1*path[j][l];
							}
							solver.addClause(new VecInt(newClause));

							attractor.add(record);
						}
						allAttractors.add(attractor);
						break;
					}
				}

				if(attractorIsFound){
					attractorIsFound = false;
				} else {
					ArrayList<ArrayList<Integer>> extendedProblem = new ArrayList<ArrayList<Integer>>();
					for(int j = i; j < i*2; j++){
						writeTimeStep( extendedProblem, j);
					}
					i *= 2;
					
					for(int k = 0; k < extendedProblem.size(); k++){
						int[] clause = new int[extendedProblem.get(k).size()];
						for(int j = 0; j < extendedProblem.get(k).size(); j++){
							clause[j] = extendedProblem.get(k).get(j);
						}
						solver.addClause(new VecInt(clause));
					}
				}
			}
		} catch (TimeoutException e){
			System.err.println("Time limit exceeded");
		} catch (ContradictionException e){
			System.err.println("Encountered a contradiction:");
			System.err.println(e.getMessage());
			e.printStackTrace();
		}
		
		System.out.println("Found " + allAttractors.size() + " attractors");
		
		for(ArrayList<ArrayList<Integer>> attractor : allAttractors){
			System.out.println("Attractor:");
			for(ArrayList<Integer> state : attractor){
				
				System.out.print("\t");
				boolean first = true;
				
				for(Map.Entry<Integer, Integer> mapping : constants.entrySet()){
					if(first){
						first = false;
					} else {
						System.out.print(" & ");
					}
				
					MDDVariable var = manager.getAllVariables()[mapping.getKey()-1];
				
					if(mapping.getValue() == 0){
						System.out.print("!");
					} 
					System.out.print(var.toString());
				}
				
				for(int node : state){
					if(first){
						first = false;
					} else {
						System.out.print(" & ");
					}

					if(node > 0){
						MDDVariable var = manager.getAllVariables()[node-1];
						System.out.print(var.toString());
					} else {
						node *= -1;
						MDDVariable var = manager.getAllVariables()[node-1];
						System.out.print("!" + var.toString());
					}
				}
				System.out.println();
			}
		}

		// FIXME: return a result object
		return null;
	}
	
	/**
	 * The SAT solver has no way of taking in constant values, so they are removed from the DNF functions here
	 */
	private void processConstants(){
		Map<Integer, Integer> allConstants = new HashMap<Integer, Integer>();
		Map<Integer, Integer> forcedConstants = constants;

		while(forcedConstants.size() > 0){
			HashMap<Integer, Integer> newForcedConstants = new HashMap<Integer, Integer>();
			for(Map.Entry<Integer, Integer> mapping : forcedConstants.entrySet()){
				allConstants.put(mapping.getKey(), mapping.getValue());
				dnfFunctions.remove(mapping.getKey());
				if(mapping.getValue() == 0){
					ArrayList<Integer> valuesToRemove = new ArrayList<Integer>();
					valuesToRemove.add(-1*mapping.getKey());
					for(Map.Entry<Integer, ArrayList<ArrayList<Integer>>> function : dnfFunctions.entrySet()){
						ArrayList<ArrayList<Integer>> clausesToRemove = new ArrayList<ArrayList<Integer>>();
						
						for(ArrayList<Integer> clause : function.getValue()){
							if(clause.contains(mapping.getKey())){
								clausesToRemove.add(clause);
							} else {
								clause.removeAll(valuesToRemove);
								if(clause.size() == 0){
									newForcedConstants.put(function.getKey(), 1);
								}
							}
						}
						function.getValue().removeAll(clausesToRemove);
						if(function.getValue().size() == 0){
							newForcedConstants.put(function.getKey(), 0);
						}
					}
				} else {
					ArrayList<Integer> valuesToRemove = new ArrayList<Integer>();
					valuesToRemove.add(mapping.getKey());
					for(Map.Entry<Integer, ArrayList<ArrayList<Integer>>> function : dnfFunctions.entrySet()){
						ArrayList<ArrayList<Integer>> clausesToRemove = new ArrayList<ArrayList<Integer>>();
					
						for(ArrayList<Integer> clause : function.getValue()){
							if(clause.contains(-1*mapping.getKey())){
								clausesToRemove.add(clause);
							} else {
								clause.removeAll(valuesToRemove);
								if(clause.size() == 0){
									newForcedConstants.put(function.getKey(), 1);
								}
							}
						}
						function.getValue().removeAll(clausesToRemove);
						if(function.getValue().size() == 0){
							newForcedConstants.put(function.getKey(), 0);
						}
					}
				}
			}
			forcedConstants = newForcedConstants;
		}
		
		for(Map.Entry<Integer, Integer> mapping : forcedConstants.entrySet()){
			allConstants.put(mapping.getKey(), mapping.getValue());
			dnfFunctions.remove(mapping.getKey());
		}
		
		constants = allConstants;
	}
	
	/**
	 * Uses dnfFunctions to create a mapping of variables to a conjunctive normal form of their function
	 * Both the disjuctive and conjunctive normal forms are used to represent functions with the ≡ operator in CNF
	 *
	 * Possible improvement: simplify the set of maxterms used for the CNF representation.
	 */
	private void disjunctiveToConjunctive(){
		for(Map.Entry<Integer, ArrayList<ArrayList<Integer>>> mapping : dnfFunctions.entrySet()){
			Map<Integer, Integer> inputs = new HashMap<Integer, Integer>();
			Map<Integer, Integer> reverseInputs = new HashMap<Integer, Integer>();
			ArrayList<ArrayList<Integer>> cnfFunction = new ArrayList<ArrayList<Integer>>();
			int index = 0;
			for(ArrayList<Integer> clause : mapping.getValue()){
				for(Integer var : clause){
					if(var < 0) {
						var *= -1;
					}
					if(!inputs.containsKey(var)){
						inputs.put(var, index);
						reverseInputs.put(index, var);
						index++;
					}
				}
			}
			
			int[] truthTable = new int[(1 << inputs.size())];
			
			for(ArrayList<Integer> clause : mapping.getValue()){
				ArrayList<Integer> notFixed = new ArrayList<Integer>(inputs.keySet());
				int baseIndex = 0;
				for(Integer var : clause) {
					if(var < 0){
						var *= -1;
					}else {
						baseIndex += (1 << inputs.get(var));
					}
					notFixed.remove(var);
				}
				for(int i = 0; i < (1 << notFixed.size()); i++){
					int variableIndex = 0;
					for(int j = 0; j < notFixed.size(); j++){
						if( ((i >> j) & 1) == 1){
							variableIndex += (1 << inputs.get(notFixed.get(j)));
						}
					}
					truthTable[baseIndex + variableIndex] = 1;
				}
			}
			
			for(int i = 0; i < truthTable.length; i++){
				if(truthTable[i] == 0){
					ArrayList<Integer> cnfClause = new ArrayList<Integer>();
					for(int j = 0; j < reverseInputs.size(); j++){
						if( ((i >> j) & 1) == 1){
							cnfClause.add(-1 * reverseInputs.get(j));
						} else {
							cnfClause.add(reverseInputs.get(j));
						}
					}
					cnfFunction.add(cnfClause);
				}
			}

			cnfFunctions.put(mapping.getKey(), cnfFunction);
		}
	}
	
	/**
	 * Writes all functions to the given ArrayList at the given point in time in conjunctive normal form.
	 * Calling this function multiple times with different time steps gives a single equation for all functions at multiple points in time
	 * 
	 * @param cnfMatrix
	 * @param timeStep
	 */
	private void writeTimeStep(ArrayList<ArrayList<Integer>> cnfMatrix, int timeStep){
		int offset = (dnfFunctions.size() + constants.size()) * timeStep;
		int futureOffset = (dnfFunctions.size() + constants.size()) * (timeStep+1);

		for(Map.Entry<Integer, ArrayList<ArrayList<Integer>>> mapping : dnfFunctions.entrySet()){
			int func = mapping.getKey();
			if(mapping.getValue().get(0).get(0) == 0){
				continue;
			}
			for(ArrayList<Integer> dnfClause : mapping.getValue()){
				ArrayList<Integer> cnfClause = new ArrayList<Integer>();
				for(int i : dnfClause){
					if(i > 0){
						cnfClause.add(-1*(i+futureOffset));
					} else {
						cnfClause.add(-1*(i-futureOffset));
					}
				}
				cnfClause.add(func + offset);
				cnfMatrix.add(cnfClause);
			}
			
			ArrayList<ArrayList<Integer>> cnfFunction = cnfFunctions.get(func);
			for(ArrayList<Integer> cnfClause : cnfFunction){
				ArrayList<Integer> newClause = new ArrayList<Integer>();
				for(int i : cnfClause){
					if(i > 0){
						newClause.add(i+futureOffset);
					} else {
						newClause.add(i-futureOffset);
					}
				}
				newClause.add(-1*(func+offset));
				cnfMatrix.add(newClause);
			}
		}
		
	}
	
	/**
	 * Parse a model to construct a set of functions in disjunctive normal form
	 * ( | ) & ( | ) & ( | ) & ...
	 * The map is 1-indexed
	 * 
	 * @param model
	 */
	private void parseModel(LogicalModel model){
		MDDManager ddmanager = model.getMDDManager();
		MDDVariable[] variables = ddmanager.getAllVariables();
		PathSearcher searcher = new PathSearcher(ddmanager);
		
		int[] functions = model.getLogicalFunctions();
		for (int idx=0 ; idx<functions.length ; idx++) {
			MDDVariable var = variables[idx];
			
			int function = functions[idx];
			
			//The SAT solver used does not have a way to input constants, so they must be
			//processed before handing the problem to the SAT solver
			if (ddmanager.isleaf(function)) {
				constants.put(idx+1, function);
				continue;
			}
			
			int[] path = searcher.setNode(function);
			ArrayList<ArrayList<Integer>> func = new ArrayList<ArrayList<Integer>>();
			for (int leaf: searcher) {
				if (leaf == 0) {
					continue;
				}
				
				ArrayList<Integer> list = new ArrayList<Integer>();

				for (int i=0 ; i<path.length ; i++) {
					int cst = path[i];
					if (cst < 0) {
						continue;
					}
					
					if (cst == 0) {
						list.add(-1*(i+1));
					} else {
						list.add(i+1);
					}
				}
				func.add(list);
			}
			dnfFunctions.put(idx+1, func);
		}
	}

	public void cli() {
		try {
			call();

			// TODO: print the results
		} catch (Exception e) {
			System.err.println("Error while searching for synchronous attractors");
		}
	}
}
