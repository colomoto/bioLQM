package org.colomoto.logicalmodel.tool.simulation.updater;

import java.util.ArrayList;
import java.util.List;

/**
 * Internal representation of a set of priority classes. Support for split
 * transitions and synchronous/asynchronous classes.
 * 
 * @author Pedro T. Monteiro
 * @author Pedro L. Varela
 */
public class PriorityClasses {
	private List<PriorityClass> priorities;

	public PriorityClasses() {
		this.priorities = new ArrayList<PriorityClass>();
	}

	public int size() {
		return this.priorities.size();
	}

	public int[] getClass(int idxPC) {
		if (!this.contains(idxPC))
			return null;
		return this.priorities.get(idxPC).array();
	}

	public boolean isSync(int idxPC) {
		if (!this.contains(idxPC))
			return true;
		return this.priorities.get(idxPC).isSync();
	}

	public void add(int[] variables, boolean sync) {
		this.priorities.add(new PriorityClass(variables, sync));
	}

	private boolean contains(int idxPC) {
		return (this.priorities.size() > idxPC);
	}

	// public boolean contains(int idxPC, int idxVar, int split) {
	// return (this.priorities.contains(idxPC) && this.priorities.get(idxPC)
	// .contains(idxVar, split));
	// }

	public void split(int idxPC, int idxVar) {
		if (!this.contains(idxPC))
			return;
		this.priorities.get(idxPC).split(idxVar);
	}

	public void unsplit(int idxPC, int idxVar, int split) {
		if (!this.contains(idxPC))
			return;
		// Format: varA,0,varC,1 | varB,0,varC,-1
		if (this.priorities.get(idxPC).unsplit(idxVar, split)) {
			int invSplit = -1 * split;
			for (PriorityClass pc : this.priorities) {
				if (pc.remove(idxVar, invSplit)) {
					if (pc.isEmpty()) {
						this.priorities.remove(pc);
					}
					return;
				}
			}
		}
	}

	public PriorityClasses clone() {
		List<PriorityClass> lPCs = new ArrayList<PriorityClass>();
		for (PriorityClass pc : this.priorities)
			lPCs.add(pc.clone());
		PriorityClasses newPCs = new PriorityClasses();
		newPCs.priorities = lPCs;
		return newPCs;
	}
}

class PriorityClass {
	private int[] variables; // Not only components since they can split
	private boolean isSync;

	public PriorityClass(int[] variables, boolean isSync) {
		this.variables = variables;
		this.isSync = isSync;
	}

	public boolean isSync() {
		return this.isSync;
	}

	public int[] array() {
		return this.variables;
	}

	public boolean isEmpty() {
		return (this.variables == null || this.variables.length < 1);
	}

	private boolean contains(int idx, int split) {
		for (int i = 0; i < this.variables.length; i += 2) {
			if (this.variables[i] == idx && this.variables[i + 1] == split)
				return true;
		}
		return false;
	}

	/**
	 * Splits a variable in two (in the current/selected class).
	 * 
	 * @param idx
	 */
	public void split(int idx) {
		if (!this.contains(idx, 0))
			return;

		int[] newVars = new int[this.variables.length + 2];
		for (int i = 0, j = 0; i < this.variables.length; i += 2, j += 2) {
			newVars[j] = this.variables[i];
			if (this.variables[i] != idx) {
				newVars[j + 1] = this.variables[i + 1];
			} else {
				newVars[j + 1] = -1;
				newVars[j + 2] = this.variables[i];
				newVars[j + 3] = 1;
				j += 2;
			}
		}
		this.variables = newVars;
	}

	/**
	 * Marks a given variable as not split. The user is responsible for finding
	 * the variable complement on the other classes (or the present one).
	 * 
	 * @param idx
	 * @param split
	 * @return
	 */
	public boolean unsplit(int idx, int split) {
		if (!this.contains(idx, split))
			return false;

		for (int i = 0; i < this.variables.length; i += 2) {
			if (this.variables[i] == idx) {
				this.variables[i + 1] = 0;
				break;
			}
		}
		return true;
	}

	/**
	 * Removes a given variable/split pair from a class, if it exists. Usually
	 * called for the complement of a previously unsplit variable.
	 * 
	 * @param idx
	 * @param split
	 * @return
	 */
	public boolean remove(int idx, int split) {
		if (!this.contains(idx, split))
			return false;

		int[] newVars = new int[this.variables.length - 2];
		for (int i = 0, j = 0; i < this.variables.length; i += 2, j += 2) {
			if (this.variables[i] == idx && this.variables[i + 1] == split) {
				i += 2;
			}
			newVars[j] = this.variables[i];
			newVars[j + 1] = this.variables[i + 1];
		}
		this.variables = newVars;
		return true;
	}

	public PriorityClass clone() {
		return new PriorityClass(this.variables.clone(), this.isSync);
	}
}