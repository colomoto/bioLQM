package org.colomoto.logicalmodel.tool.simulation.updater;

import java.util.ArrayList;
import java.util.Arrays;
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

	public void split(int idxPC, int idxVar) {
		if (!this.contains(idxPC))
			return;
		this.priorities.get(idxPC).split(idxVar);
	}

	public void unsplit(int idxPC, int idxVar, int split) {
		if (!this.contains(idxPC))
			return;
		// Format: [varA,0,varC,1] [varB,0,varC,-1]
		if (this.priorities.get(idxPC).unsplit(idxVar, split)) {
			// Then it removes the complement split
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

	public void incPriority(int idxPC, int idxVar, int split) {
		if (idxPC <= 0 || !this.contains(idxPC))
			return;
		this.priorities.get(idxPC).remove(idxVar, split);
		this.priorities.get(idxPC - 1).add(idxVar, split);
		if (this.priorities.get(idxPC).array().length == 0) {
			this.priorities.remove(idxPC);
		}
	}

	public void decPriority(int idxPC, int idxVar, int split) {
		if (!this.contains(idxPC))
			return;
		this.priorities.get(idxPC).remove(idxVar, split);
		if ((idxPC + 1) == this.priorities.size()) {
			this.priorities.add(new PriorityClass(new int[0], this.priorities
					.get(idxPC).isSync()));
		}
		this.priorities.get(idxPC + 1).add(idxVar, split);
		if (this.priorities.get(idxPC).array().length == 0) {
			this.priorities.remove(idxPC);
		}
	}

	public void collapse(boolean isSync) {
		int sz = 0;
		for (PriorityClass pc : this.priorities) {
			sz += pc.array().length;
		}
		int[] newVars = new int[sz];
		int x = 0;
		for (PriorityClass pc : this.priorities) {
			for (int i = 0; i < pc.array().length; i++, x++) {
				newVars[x] = pc.array()[i];
			}
		}
		this.priorities = new ArrayList<PriorityClass>();
		this.priorities.add(new PriorityClass(newVars, isSync));
	}

	public PriorityClasses clone() {
		List<PriorityClass> lPCs = new ArrayList<PriorityClass>();
		for (PriorityClass pc : this.priorities)
			lPCs.add(pc.clone());
		PriorityClasses newPCs = new PriorityClasses();
		newPCs.priorities = lPCs;
		return newPCs;
	}

	public boolean equals(Object o) {
		PriorityClasses outPCs = (PriorityClasses) o;
		for (int i = 0; i < this.priorities.size(); i++) {
			if (!this.priorities.get(i).equals(outPCs.priorities.get(i)))
				return false;
		}
		return true;
	}

	private class PriorityClass {
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
		 * Marks a given variable as not split. The caller method is responsible
		 * for finding the variable complement on the other classes (or the
		 * present one).
		 * 
		 * @param idx
		 * @param split
		 * @return
		 */
		public boolean unsplit(int idx, int split) {
			if (!this.contains(idx, split))
				return false;

			for (int i = 0; i < this.variables.length; i += 2) {
				if (this.variables[i] == idx && this.variables[i + 1] == split) {
					this.variables[i + 1] = 0;
					break;
				}
			}
			return true;
		}

		/**
		 * Removes a given variable/split pair from a class, if it exists.
		 * Usually called for the complement of a previously unsplit variable.
		 * 
		 * @param idx
		 * @param split
		 * @return
		 */
		public boolean remove(int idx, int split) {
			if (!this.contains(idx, split))
				return false;

			int[] newVars = new int[this.variables.length - 2];
			for (int i = 0, j = 0; i < this.variables.length; i += 2) {
				if (this.variables[i] == idx && this.variables[i + 1] == split) {
					continue;
				}
				newVars[j] = this.variables[i];
				newVars[j + 1] = this.variables[i + 1];
				j += 2;
			}
			this.variables = newVars;
			return true;
		}

		public boolean add(int idx, int split) {
			if (this.contains(idx, split))
				return false;

			int[] newVars = new int[this.variables.length + 2];
			System.arraycopy(this.variables, 0, newVars, 0,
					this.variables.length);
			newVars[this.variables.length] = idx;
			newVars[this.variables.length + 1] = split;
			this.variables = newVars;
			return true;
		}

		public PriorityClass clone() {
			return new PriorityClass(this.variables.clone(), this.isSync);
		}

		public boolean equals(Object o) {
			PriorityClass outPC = (PriorityClass) o;
			return (this.isSync == outPC.isSync && Arrays.equals(
					this.variables, outPC.variables));
		}
	}
}