package org.colomoto.biolqm.tool.simulation.grouping;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

/**
 * @author Pedro T. Monteiro
 * @author Pedro L. Varela
 */
public class ModelGrouping {

	public static final String INC = "[+]";
	public static final String DEC = "[-]";
	public static final String SEPVAR = ",";
	public static final String SEPGROUP = "/";
	public static final String SEPCLASS = ":";

	protected LogicalModel model;
	private List<RankedClass> pcList;


	public ModelGrouping(LogicalModel m) {
		this.model = m;
		this.pcList = new ArrayList<>();

		// Init single class with a single synchronous group
		int coreVars = 0;
		for (int n = 0; n < this.model.getComponents().size(); n++) {
			if (this.model.getComponents().get(n).isInput())
				continue;
			coreVars++;
		}
		// Ignoring input vars
		int[] vars = new int[coreVars * 2];
		for (int n = 0, i = 0; n < this.model.getComponents().size(); n++) {
			if (this.model.getComponents().get(n).isInput())
				continue;
			vars[i] = n; // FIXME
			vars[i + 1] = 0;
			i += 2;
		}
		this.pcList.add(new RankedClass(new RankedClassGroup(vars)));
	}

	// OK
	public ModelGrouping(LogicalModel m, String textFormat) {
		this.model = m;
		this.pcList = new ArrayList<>();
		String[] saPCs = textFormat.split(SEPCLASS);
		for (String sPC : saPCs) {
			this.pcList.add(new RankedClass(m, sPC));
		}
	}

	// OK
	private ModelGrouping(LogicalModel m, List<RankedClass> pcList) {
		this.model = m;
		this.pcList = pcList;
	}

	// OK
	public LogicalModel getModel() {
		return this.model;
	}


	public int[][] getDeterministicBlocks() {
		int n = this.pcList.size();
		int[][] blocks = new int[n][];
		int idx = 0;
		for (RankedClass cl: this.pcList) {
			int l = 0;
			for (RankedClassGroup grp: cl.groups) {
				l += grp.vars.length;
			}
			int[] cur = new int[l];
			int pos = 0;
			for (RankedClassGroup grp: cl.groups) {
				System.arraycopy(grp.vars, 0, cur, pos, grp.vars.length);
				pos += grp.vars.length;
			}
			blocks[idx] = cur;
			idx++;
		}
		return blocks;
	}

	// OK
	public ModelGrouping clone() {
		List<RankedClass> pcNew = new ArrayList<RankedClass>();
		for (RankedClass pc : this.pcList) {
			pcNew.add(pc.clone());
		}
		return new ModelGrouping(this.model, pcNew);
	}

	public void switchClasses(int i, int j) {
		RankedClass pc = this.getClass(i);
		this.pcList.remove(i);
		this.pcList.add(j, pc);
	}

	// OK
	private boolean isValid(int idxPC) {
		return idxPC >= 0 && this.pcList.size() > idxPC;
	}

	// OK
	public RankedClass getClass(int idxPC) {
		if (!this.isValid(idxPC))
			return null;
		return this.pcList.get(idxPC);
	}

	// OK
	public List<List<String>> getClassVars(int idxPC) {
		List<List<String>> tmp;
		if (this.isValid(idxPC)) {
			tmp = this.pcList.get(idxPC).getVars(this.model);
		} else {
			tmp = new ArrayList<>();
			tmp.add(new ArrayList<>());
		}
		return tmp;
	}

	// OK
	public void decPriorities(int idxPC, int idxGrp, List<String> vars) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return;
		for (String varMm : vars) {
			int splitFlag = 0;
			String var = varMm;
			if (varMm.endsWith(INC)) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - INC.length());
			} else if (varMm.endsWith(DEC)) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - DEC.length());
			}
			for (int idx = 0; idx < this.model.getComponents().size(); idx++) {
				// If var is valid
				if (this.model.getComponents().get(idx).getNodeID().equals(var)) {
					// If group contains var
					if (this.pcList.get(idxPC).contains(idxGrp, idx, splitFlag)) {
						this.pcList.get(idxPC).remove(idxGrp, idx, splitFlag);
					}
					// If a new class is needed
					if ((idxPC + 1) == this.pcList.size()) {
						int[] newvars = new int[2];
						newvars[0] = idx;
						newvars[1] = splitFlag;
						this.pcList.add(new RankedClass(new RankedClassGroup(newvars)));
					} else {
						this.pcList.get(idxPC + 1).add(0, idx, splitFlag);
					}
				}
			}
			// If the old class is empty
			if (this.pcList.get(idxPC).isEmpty()) {
				this.pcList.remove(idxPC);
			}
		}
	}

	public void decGroup(int idxPC, int idxGrp, List<String> vars) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return;
		for (String varMm : vars) {
			int splitFlag = 0;
			String var = varMm;
			if (varMm.endsWith(INC)) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - INC.length());
			} else if (varMm.endsWith(DEC)) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - DEC.length());
			}
			for (int idx = 0; idx < this.model.getComponents().size(); idx++) {
				// If var is valid
				if (this.model.getComponents().get(idx).getNodeID().equals(var)) {
					// If group contains var
					if (this.pcList.get(idxPC).contains(idxGrp, idx, splitFlag)) {
						this.pcList.get(idxPC).remove(idxGrp, idx, splitFlag);
						if ((idxGrp + 1) == this.pcList.get(idxPC).size()) {
							int[] newVars = new int[2];
							newVars[0] = idx;
							newVars[1] = splitFlag;
							this.pcList.get(idxPC).addGrp(idxGrp + 1, new RankedClassGroup(newVars));
						} else {
							this.pcList.get(idxPC).add(idxGrp + 1, idx, splitFlag);
						}
					}
				}
			}
		}
		// If the old group is empty
		if (this.pcList.get(idxPC).groups.get(idxGrp).isEmpty()) {
			this.pcList.get(idxPC).groups.remove(idxGrp);
		}
	}

	public void incGroup(int idxPC, int idxGrp, List<String> vars) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return;
		for (String varMm : vars) {
			int splitFlag = 0;
			String var = varMm;
			if (varMm.endsWith(INC)) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - INC.length());
			} else if (varMm.endsWith(DEC)) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - DEC.length());
			}
			for (int idx = 0; idx < this.model.getComponents().size(); idx++) {
				// If var is valid
				if (this.model.getComponents().get(idx).getNodeID().equals(var)) {
					// If group contains var
					if (this.pcList.get(idxPC).contains(idxGrp, idx, splitFlag)) {
						this.pcList.get(idxPC).remove(idxGrp, idx, splitFlag);
						if (idxGrp == 0) {
							int[] newVars = new int[2];
							newVars[0] = idx;
							newVars[1] = splitFlag;
							this.pcList.get(idxPC).addGrp(0, new RankedClassGroup(newVars));
							idxGrp++;
						} else {
							this.pcList.get(idxPC).add(idxGrp - 1, idx, splitFlag);
						}
					}
				}
			}
		}
		// If the old group is empty
		if (this.pcList.get(idxPC).groups.get(idxGrp).isEmpty()) {
			this.pcList.get(idxPC).groups.remove(idxGrp);
		}
	}

	// OK
	public void incPriorities(int idxPC, int idxGrp, List<String> vars) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return;
		List<int[]> lVars = new ArrayList<int[]>();
		for (String varMm : vars) {
			int splitFlag = 0;
			String var = varMm;
			if (varMm.endsWith(INC)) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - INC.length());
			} else if (varMm.endsWith(DEC)) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - DEC.length());
			}
			for (int idx = 0; idx < this.model.getComponents().size(); idx++) {
				// If var is valid
				if (this.model.getComponents().get(idx).getNodeID().equals(var)) {
					// If group contains var
					if (this.pcList.get(idxPC).contains(idxGrp, idx, splitFlag)) {
						int[] newvars = new int[2];
						newvars[0] = idx;
						newvars[1] = splitFlag;
						lVars.add(newvars);
					}
				}
			}
		}
		if (lVars.isEmpty())
			return;
		// For all existing in the group
		for (int[] newvars : lVars) {
			this.pcList.get(idxPC).remove(idxGrp, newvars[0], newvars[1]);
			// If a new class is needed
			if (idxPC == 0) {
				this.pcList.add(0, new RankedClass(new RankedClassGroup(newvars)));
				idxPC++;
			} else {
				this.pcList.get(idxPC - 1).add(0, newvars[0], newvars[1]);
			}
		}
		// If the old group is empty
		if (this.pcList.get(idxPC).groups.get(idxGrp).isEmpty()) {
			this.pcList.get(idxPC).groups.remove(idxGrp);
		}
		// If the old class is empty
		if (this.pcList.get(idxPC).isEmpty()) {
			this.pcList.remove(idxPC);
		}
	}

	public void groupExpand(int idxPC) {
		this.pcList.get(idxPC).expand();
	}

	public void groupCollapse(int idxPC) {
		this.pcList.get(idxPC).collapse();
	}

	// OK
	public void collapseAll() {
		RankedClass pc0 = this.pcList.get(0);
		for (int c = this.size() - 1; c > 0; c--) {
			while (this.pcList.get(c).size() > 0) {
				RankedClassGroup pcg = this.pcList.get(c).removeGrp(0);
				pc0.addGrp(0, pcg);
			}
			this.pcList.remove(c);
		}
		pc0.collapse();
		// Collapses also all split variables
		for (int idxVar = 0; idxVar < this.model.getComponents().size(); idxVar++) {
			pc0.unsplit(0, idxVar, 1);
			this.remove(idxVar, -1);
		}
	}

	/**
	 * It's the number of classes
	 *
	 * @return
	 */
	public int size() {
		return this.pcList.size();
	}

	// OK
	public void split(int idxPC, int idxGrp, String var) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return;
		for (int idx = 0; idx < this.model.getComponents().size(); idx++) {
			if (this.model.getComponents().get(idx).getNodeID().equals(var)) {
				this.pcList.get(idxPC).split(idxGrp, idx);
				return;
			}
		}
	}

	// OK
	public void unsplit(int idxPC, int idxGrp, String varMm) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp)
				|| !varMm.endsWith(INC) && !varMm.endsWith(DEC))
			return;
		String var = varMm.substring(0, varMm.length() - INC.length());
		int splitFlag = varMm.endsWith(INC) ? 1 : -1;
		for (int idx = 0; idx < this.model.getComponents().size(); idx++) {
			if (this.model.getComponents().get(idx).getNodeID().equals(var)) {
				this.pcList.get(idxPC).unsplit(idxGrp, idx, splitFlag);
				this.remove(idx, -1 * splitFlag);
				break;
			}
		}
	}

	private void remove(int idxVar, int splitFlag) {
		for (int i = 0; i < this.size(); i++) {
			if (this.pcList.get(i).remove(idxVar, splitFlag)) {
				break;
			}
		}
	}

	// OK TODO
	public boolean equals(Object a) {
		ModelGrouping outMPC = (ModelGrouping) a;
		if (outMPC.getModel() != this.getModel() || outMPC.size() != this.size()) {
			return false;
		}
		for (int i = 0; i < this.size(); i++) {
			if (!outMPC.pcList.get(i).equals(this.pcList.get(i))) {
				return false;
			}
		}
		return true;
	}

	public String toString() {
		String sTmp = "";
		for (RankedClass pc : this.pcList) {
			if (!sTmp.isEmpty())
				sTmp += SEPCLASS;
			sTmp += pc;
		}
		return sTmp;
	}

	/**
	 *
	 * @author Pedro T. Monteiro
	 * @author Pedro L. Varela
	 *
	 */
	public class RankedClass {

		private List<RankedClassGroup> groups;

		public RankedClass(RankedClassGroup pcg) {
			this.groups = new ArrayList<>();
			this.groups.add(pcg);
		}

		// OK
		public RankedClass(LogicalModel m, String textFormat) {
			this.groups = new ArrayList<>();
			String[] saPCGs = textFormat.split(SEPGROUP);
			for (String sPCG : saPCGs) {
				this.groups.add(new RankedClassGroup(m, sPCG));
			}
		}

		// OK
		public boolean isValid(int idxGrp) {
			return idxGrp >= 0 && this.groups.size() > idxGrp;
		}

		public int size() {
			return this.groups.size();
		}

		public boolean isEmpty() {
			return this.size() == 0 || (this.size() == 1 && this.groups.get(0).isEmpty());
		}

		private boolean contains(int idxGrp, int idxVar, int splitFlag) {
			return this.groups.get(idxGrp).contains(idxVar, splitFlag);
		}

		public RankedClassGroup removeGrp(int idxGrp) {
			if (!this.isValid(idxGrp))
				return null;
			RankedClassGroup pcg = this.groups.get(idxGrp);
			this.groups.remove(idxGrp);
			return pcg;
		}

		public void addGrp(int pos, RankedClassGroup pcg) {
			this.groups.add(pos, pcg);
		}

		public int[] getGroupValues(int idxGrp) {
			if (idxGrp >= 0 && idxGrp < this.size()) {
				return this.groups.get(idxGrp).array();
			}
			return null;
		}

		// OK
		public List<List<String>> getVars(LogicalModel m) {
			List<List<String>> lVars = new ArrayList<List<String>>();
			for (RankedClassGroup pcg : this.groups) {
				lVars.add(pcg.getVars(m));
			}
			return lVars;
		}

		// OK
		public void collapse() {
			for (int g = this.size() - 1; g > 0; g--) {
				int[] vars = this.getGroupValues(g);
				for (int i = 0; i < vars.length; i += 2) {
					this.groups.get(0).add(vars[i], vars[i + 1]);
				}
				this.groups.remove(g);
			}
		}

		// OK
		public void expand() {
			List<RankedClassGroup> lPCGs = new ArrayList<RankedClassGroup>();
			for (RankedClassGroup pcg : this.groups) {
				int[] vars = pcg.array();
				for (int i = 0; i < vars.length; i += 2) {
					int[] newVars = new int[2];
					newVars[0] = vars[i];
					newVars[1] = vars[i + 1];
					RankedClassGroup pcgNew = new RankedClassGroup(newVars);
					lPCGs.add(pcgNew);
				}
			}
			this.groups = lPCGs;
		}

		/**
		 * Splits a variable in two (in the current/selected class).
		 *
		 * @param idxGrp
		 * @param idxVar
		 */
		public boolean split(int idxGrp, int idxVar) {
			return this.groups.get(idxGrp).split(idxVar);
		}

		/**
		 * Marks a given variable as not split. The caller method is responsible for
		 * finding the variable complement on the other classes (or the present one).
		 *
		 * @param idxGrp
		 * @param idxVar
		 * @param splitFlag
		 * @return
		 */
		public boolean unsplit(int idxGrp, int idxVar, int splitFlag) {
			return this.groups.get(idxGrp).unsplit(idxVar, splitFlag);
		}

		/**
		 * Removes a given variable/split pair from a class, if it exists. Usually
		 * called for the complement of a previously unsplit variable.
		 *
		 * @param idxGrp
		 * @param idxVar
		 * @param splitFlag
		 * @return
		 */
		public boolean remove(int idxGrp, int idxVar, int splitFlag) {
			return this.groups.get(idxGrp).remove(idxVar, splitFlag);
		}

		public boolean remove(int idxVar, int splitFlag) {
			for (int g = 0; g < this.groups.size(); g++) {
				if (this.remove(g, idxVar, splitFlag)) {
					return true;
				}
			}
			return false;
		}

		public boolean add(int idxGrp, int idxVar, int splitFlag) {
			return this.groups.get(idxGrp).add(idxVar, splitFlag);
		}

		public RankedClass clone() {
			RankedClass pc = new RankedClass(this.groups.get(0).clone());
			for (int g = 1; g < this.groups.size(); g++) {
				pc.addGrp(g, this.groups.get(g).clone());
			}
			return pc;
		}

		/**
		 * Considers both classes with same ordered groups
		 */
		public boolean equals(Object o) {
			RankedClass outPC = (RankedClass) o;
			if (outPC.size() != this.size())
				return false;
			for (int i = 0; i < this.size(); i++) {
				if (!outPC.groups.get(i).equals(this.groups.get(i)))
					return false;
			}
			return true;
		}

		public String toString() {
			String sPC = "";
			for (RankedClassGroup pcg : this.groups) {
				if (!sPC.isEmpty())
					sPC += SEPGROUP;
				String sG = "";
				for (int i = 0; i < pcg.vars.length; i += 2) {
					if (!sG.isEmpty())
						sG += SEPVAR;
					sG += model.getComponents().get(pcg.vars[i]).getNodeID();
					if (pcg.vars[i + 1] == 1) {
						sG += INC;
					} else if (pcg.vars[i + 1] == -1) {
						sG += DEC;
					}
				}
				sPC += sG;
			}
			return sPC;
		}
	}

	/**
	 *
	 * @author Pedro T. Monteiro
	 * @author Pedro L. Varela
	 *
	 */
	public class RankedClassGroup {
		// 2*n positions for n variables
		private int[] vars;

		public RankedClassGroup(int[] vars) {
			this.vars = vars;
		}

		// OK
		public RankedClassGroup(LogicalModel m, String textFormat) {
			String[] saVars = textFormat.split(SEPVAR);
			List<int[]> lVars = new ArrayList<int[]>();

			vars: for (int i = 0; i < saVars.length; i++) {
				String var = saVars[i];
				int split = 0;
				if (saVars[i].endsWith(DEC)) {
					split = -1;
					var = saVars[i].substring(0, var.length() - DEC.length());
				} else if (saVars[i].endsWith(INC)) {
					split = 1;
					var = saVars[i].substring(0, var.length() - INC.length());
				}
				for (int idx = 0; idx < m.getComponents().size(); idx++) {
					NodeInfo node = m.getComponents().get(idx);
					if (node.getNodeID().equals(var)) {
						if (node.isInput()) {
							continue vars;
						} else {
							int[] newVar = new int[2];
							newVar[0] = idx;
							newVar[1] = split;
							lVars.add(newVar);
							break;
						}
					}
				}
			}
			this.vars = new int[lVars.size() * 2];
			for (int i = 0; i < lVars.size(); i++) {
				this.vars[i * 2] = lVars.get(i)[0];
				this.vars[i * 2 + 1] = lVars.get(i)[1];
			}
		}

		public int[] array() {
			return this.vars;
		}

		public boolean isEmpty() {
			return (this.vars == null || this.vars.length < 1);
		}

		public int size() {
			return this.vars.length / 2;
		}

		private boolean contains(int idx, int splitFlag) {
			for (int i = 0; i < this.vars.length; i += 2) {
				if (this.vars[i] == idx && this.vars[i + 1] == splitFlag) {
					return true;
				}
			}
			return false;
		}

		// OK
		public List<String> getVars(LogicalModel m) {
			List<String> lVars = new ArrayList<String>();
			for (int i = 0; i < this.vars.length; i += 2) {
				String var = m.getComponents().get(this.vars[i]).getNodeID();
				if (this.vars[i + 1] == 1) {
					var += INC;
				} else if (this.vars[i + 1] == -1) {
					var += DEC;
				}
				lVars.add(var);
			}
			return lVars;
		}

		/**
		 * Splits a variable in two (in the current/selected group).
		 *
		 * @param idx
		 */
		public boolean split(int idx) {
			if (!this.contains(idx, 0))
				return false;

			int[] newVars = new int[this.vars.length + 2];
			for (int i = 0, j = 0; i < this.vars.length; i += 2, j += 2) {
				newVars[j] = this.vars[i];
				if (this.vars[i] != idx) {
					newVars[j + 1] = this.vars[i + 1];
				} else {
					newVars[j + 1] = -1;
					newVars[j + 2] = this.vars[i];
					newVars[j + 3] = 1;
					j += 2;
				}
			}
			this.vars = newVars;
			return true;
		}

		/**
		 * Marks a given variable as not split. It is the responsibility of the caller
		 * method to find the complement variable on the other classes (or the present
		 * one).
		 *
		 * @param idx
		 * @param splitFlag
		 * @return
		 */
		public boolean unsplit(int idx, int splitFlag) {
			if (!this.contains(idx, splitFlag))
				return false;

			for (int i = 0; i < this.vars.length; i += 2) {
				if (this.vars[i] == idx && this.vars[i + 1] == splitFlag) {
					this.vars[i + 1] = 0;
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
		 * @param splitFlag
		 * @return
		 */
		public boolean remove(int idx, int splitFlag) {
			if (!this.contains(idx, splitFlag))
				return false;

			int[] newVars = new int[this.vars.length - 2];
			for (int i = 0, j = 0; i < this.vars.length; i += 2) {
				if (this.vars[i] == idx && this.vars[i + 1] == splitFlag) {
					continue;
				}
				newVars[j] = this.vars[i];
				newVars[j + 1] = this.vars[i + 1];
				j += 2;
			}
			this.vars = newVars;
			return true;
		}

		public boolean add(int idx, int splitFlag) {
			if (this.contains(idx, splitFlag))
				return false;

			int[] newVars = new int[this.vars.length + 2];
			System.arraycopy(this.vars, 0, newVars, 0, this.vars.length);
			newVars[this.vars.length] = idx;
			newVars[this.vars.length + 1] = splitFlag;
			this.vars = newVars;
			return true;
		}

		public RankedClassGroup clone() {
			return new RankedClassGroup(this.vars.clone());
		}

		public boolean equals(Object o) {
			RankedClassGroup outPC = (RankedClassGroup) o;
			return Arrays.equals(this.vars, outPC.vars);
		}
	}
}
