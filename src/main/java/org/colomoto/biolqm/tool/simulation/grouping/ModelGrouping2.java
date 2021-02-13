package org.colomoto.biolqm.tool.simulation.grouping;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;
import org.hamcrest.core.IsInstanceOf;
/**
 * @author Pedro T. Monteiro
 * @author Pedro L. Varela
 */
public class ModelGrouping2 {

	public static final String SEPVAR = ",";
	public static final String SEPGROUP = "/";
	public static final String SEPCLASS = ":";
	
	public static final String SEPUPDATER = "$";
	public static final String[] updatersList = 
			new String[] {"Random uniform", "Random non uniform", "Synchronous"};

	protected LogicalModel model;
	private List<RankedClass> pcList;


	public ModelGrouping2(LogicalModel m) { 
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

	public ModelGrouping2(LogicalModel m, String textFormat) {
		this.model = m;
		this.pcList = new ArrayList<>();
		String[] saPCs = textFormat.split(SEPCLASS);
		for (String sPC : saPCs) {
			this.pcList.add(new RankedClass(m, sPC));
		}
	}

	private ModelGrouping2(LogicalModel m, List<RankedClass> pcList) {
		this.model = m;
		this.pcList = pcList;
	}
	

	public LogicalModel getModel() {
		return this.model;
	}

	public int[][] getDeterministicBlocks() {
		int n = this.pcList.size();
		int[][] blocks = new int[n][];
		int idx = 0;
		for (RankedClass cl : this.pcList) {
			int l = 0;
			for (RankedClassGroup grp : cl.groups) {
				l += grp.vars.length;
			}
			int[] cur = new int[l];
			int pos = 0;
			for (RankedClassGroup grp : cl.groups) {
				System.arraycopy(grp.vars, 0, cur, pos, grp.vars.length);
				pos += grp.vars.length;
			}
			blocks[idx] = cur;
			idx++;
		}
		return blocks;
	}

	@Override
	public ModelGrouping2 clone() {
		List<RankedClass> pcNew = new ArrayList<RankedClass>();
		for (RankedClass pc : this.pcList) {
			pcNew.add(pc.clone());
		}
		return new ModelGrouping2(this.model, pcNew);
	}
	
	// FIXME: hack for EpiLog to pass modified/perturbed models to the PriorityUpdater
	public ModelGrouping2 cloneWithModel(LogicalModel modifiedModel) {
		List<RankedClass> pcNew = new ArrayList<RankedClass>();
		for (RankedClass pc : this.pcList) {
			pcNew.add(pc.clone());
		} 
		return new ModelGrouping2(modifiedModel, pcNew);
	}

	public void switchClasses(int i, int j) {
		RankedClass pc = this.getClass(i);
		this.pcList.remove(i);
		this.pcList.add(j, pc);
	}

	private boolean isValid(int idxPC) {
		return idxPC >= 0 && this.pcList.size() > idxPC;
	}

	public RankedClass getClass(int idxPC) {
		if (!this.isValid(idxPC))
			return null;
		return this.pcList.get(idxPC);
	}

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
	
	public static String[] getUpdatersAvailable() {
		return ModelGrouping2.updatersList;
	}

	public void decPriorities(int idxPC, int idxGrp, List<String> vars) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return;
		for (String varMm : vars) {
			int splitFlag = 0;
			String var = varMm;
			if (varMm.endsWith(SplittingType.POSITIVE.toString())) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - SplittingType.POSITIVE.toString().length());
			} else if (varMm.endsWith(SplittingType.NEGATIVE.toString())) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - SplittingType.NEGATIVE.toString().length());
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
			if (varMm.endsWith(SplittingType.POSITIVE.toString())) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - SplittingType.POSITIVE.toString().length());
			} else if (varMm.endsWith(SplittingType.NEGATIVE.toString())) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - SplittingType.NEGATIVE.toString().length());
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
			if (varMm.endsWith(SplittingType.POSITIVE.toString())) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - SplittingType.POSITIVE.toString().length());
			} else if (varMm.endsWith(SplittingType.NEGATIVE.toString())) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - SplittingType.NEGATIVE.toString().length());
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
							// if it was in the top group, create a new one and make it top
							// add variable to it
							this.pcList.get(idxPC).addGrp(0, new RankedClassGroup(newVars));
							idxGrp++;
						} else {
							// if it was in a non top group, add the variable to the previous
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

	public void incPriorities(int idxPC, int idxGrp, List<String> vars) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return;
		List<int[]> lVars = new ArrayList<int[]>();
		for (String varMm : vars) {
			int splitFlag = 0;
			String var = varMm;
			if (varMm.endsWith(SplittingType.POSITIVE.toString())) {
				splitFlag = 1;
				var = varMm.substring(0, varMm.length() - SplittingType.POSITIVE.toString().length());
			} else if (varMm.endsWith(SplittingType.NEGATIVE.toString())) {
				splitFlag = -1;
				var = varMm.substring(0, varMm.length() - SplittingType.NEGATIVE.toString().length());
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
	
	public String getGroupUpdaterName(int idxPC, int idxGrp) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return "";
		return this.pcList.get(idxPC).getGroupUpdaterName(idxGrp);
	}
	
	public LogicalModelUpdater getUpdater(int idxPC, int idxGrp) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp))
			return null;
		return this.pcList.get(idxPC).getUpdater(idxGrp);
	}
	
	public void addUpdater(int idxPC, int idxGrp, LogicalModelUpdater updater) {
		if (this.isValid(idxPC) && this.pcList.get(idxPC).isValid(idxGrp))
			this.pcList.get(idxPC).addUpdater(idxGrp, updater);
	}
	
	public void addUpdater(int idxPC, int idxGrp, Map<String, Double> rates) {
		if (this.isValid(idxPC) && this.pcList.get(idxPC).isValid(idxGrp))
			this.pcList.get(idxPC).addUpdater(idxGrp, rates);
	}
	
	public double[] getRates(int idxPC, int idxGrp) {
		if (this.isValid(idxPC) && this.pcList.get(idxPC).isValid(idxGrp))
			return this.pcList.get(idxPC).getRates(idxGrp);
		return new double[0];
	}

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

	public void unsplit(int idxPC, int idxGrp, String varMm) {
		if (!this.isValid(idxPC) || !this.pcList.get(idxPC).isValid(idxGrp)
				|| !varMm.endsWith(SplittingType.POSITIVE.toString())
						&& !varMm.endsWith(SplittingType.NEGATIVE.toString()))
			return;
		String var = varMm.substring(0, varMm.length() - SplittingType.POSITIVE.toString().length());
		int splitFlag = varMm.endsWith(SplittingType.POSITIVE.toString()) ? 1 : -1;
		for (int idx = 0; idx < this.model.getComponents().size(); idx++) {
			if (this.model.getComponents().get(idx).getNodeID().equals(var)) {
	
				this.pcList.get(idxPC).unsplit(idxGrp, idx, splitFlag);
				
				// remove a flag complementar
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
	
	public void accept() {
		for (int i = 0; i < this.size(); i++) {
			this.pcList.get(i).accept();
		}
	}
	
	@Override
	public boolean equals(Object a) {
		ModelGrouping2 outMPC = (ModelGrouping2) a;
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
			sTmp += pc.toString();
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



		public RankedClass(LogicalModel m, String textFormat) {
			this.groups = new ArrayList<>();
			String[] saPCGs = textFormat.split(SEPGROUP);
			for (String sPCG : saPCGs) {
				this.groups.add(new RankedClassGroup(m, sPCG));
			}
		}

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
		
		public String getGroupUpdaterName(int idxGrp) {
			if (idxGrp >= 0 && idxGrp < this.size()) {
				return this.groups.get(idxGrp).getUpdaterName();
			}
			return "";
		}
		
		public LogicalModelUpdater getUpdater(int idxGrp) {
			if (idxGrp >= 0 && idxGrp < this.size()) {
				return this.groups.get(idxGrp).getUpdater();
			}
			return null;
		}
		
		public void addUpdater(int idxGrp, LogicalModelUpdater updater) {
			if (idxGrp >= 0 && idxGrp < this.size()) {
				this.groups.get(idxGrp).addUpdater(updater);
			}
		}
		
		public void addUpdater(int idxGrp, Map<String, Double> rates ) {
			if (idxGrp >= 0 && idxGrp < this.size()) {
				this.groups.get(idxGrp).addUpdater(rates);
			}
		}
		
		public double[] getRates(int idxGrp) {
			if (idxGrp >= 0 && idxGrp < this.size()) 
				return this.groups.get(idxGrp).getRates();
			return new double[0];
		}
			
				
		public List<List<String>> getVars(LogicalModel m) {
			List<List<String>> lVars = new ArrayList<List<String>>();
			for (RankedClassGroup pcg : this.groups) {
				lVars.add(pcg.getVars(m));
			}
			return lVars;
		}

		public void collapse() {
			for (int g = this.size() - 1; g > 0; g--) {
				int[] vars = this.getGroupValues(g);
				for (int i = 0; i < vars.length; i += 2) {
					this.groups.get(0).add(vars[i], vars[i + 1]);
				}
				this.groups.remove(g);
			}
		}

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
		 * @param model TODO
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
		
		public void accept() {
			for (int g = 0; g < this.groups.size(); g++) {
				this.groups.get(g).updateVarsAndFilter();
			}
			
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
						sG += SplittingType.POSITIVE.toString();
					} else if (pcg.vars[i + 1] == -1) {
						sG += SplittingType.NEGATIVE.toString();
					}
				}
				sPC += sG;
				sPC += pcg.getUdaterString();
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
		private Map<NodeInfo, SplittingType> filter;
		private LogicalModelUpdater updater;
		private String updaterString;
		
		
		public RankedClassGroup(int[] vars) {
			this.vars = vars;
			this.updater = new SynchronousUpdater(model);
			this.updaterString = "";
			updateVarsAndFilter();
		}
		
		public RankedClassGroup(int[] vars, LogicalModelUpdater updater) {
			this.vars = vars;
			addUpdater(updater);
		}
		
		public RankedClassGroup(LogicalModel m, String textFormat) {
			String[] up = textFormat.split("\\" + SEPUPDATER);
			String[] saVars = up[0].split(SEPVAR);
			List<int[]> lVars = new ArrayList<int[]>();

			vars: for (int i = 0; i < saVars.length; i++) {
				String var = saVars[i];				
				int split = 0;
				if (saVars[i].endsWith(SplittingType.NEGATIVE.toString())) {
					split = -1;
					var = saVars[i].substring(0, var.length() - SplittingType.NEGATIVE.toString().length());
				} else if (saVars[i].endsWith(SplittingType.POSITIVE.toString())) {
					split = 1;
					var = saVars[i].substring(0, var.length() - SplittingType.POSITIVE.toString().length());
				}
				// last variable, should be followed by $RU, $RN, $S or "".
				for (int idx = 0; idx < m.getComponents().size(); idx++) {
					NodeInfo node = m.getComponents().get(idx);
					// find Node with var nodeID
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
				
				// idx, split, idx, split ...
				this.vars[i * 2] = lVars.get(i)[0];
				this.vars[i * 2 + 1] = lVars.get(i)[1];
			}
			
			// get the filter
			this.updateVarsAndFilter();
						
			// get updater
			if (up.length == 1) {
				// default, Synchronous
				this.updaterString = "";
				this.updater = new SynchronousUpdater(model);
			} else if (up.length == 2) {
				// either $S, $RN or $RU
				if (up[1].equals("S")) {
					this.updaterString = SEPUPDATER + "S";
					this.updater = new SynchronousUpdater(model);
					
				} else if (up[1].equals("RU")) {
					this.updaterString = SEPUPDATER + "RU";
					MultipleSuccessorsUpdater MultiUpdater = new AsynchronousUpdater(model);
					this.updater = new RandomUpdaterWrapper(MultiUpdater);
					
				} else if (up[1].equals("C")) {
					this.updaterString = SEPUPDATER + "C";
					// ?? 
				} else if (up[1].equals("BS")) {
					this.updaterString = SEPUPDATER + "BS";
					// ?? 
				} else if (up[1].equals("A")) {
					this.updaterString = SEPUPDATER + "A";
					// ?? 
				} else {
					this.updaterString =  SEPUPDATER + "RN";
					// $RN[0.3,0.5,...], get rates
					String[] rates = up[1].substring(3, up[1].length() - 1).split(",");
					double[] doubleRates = new double[rates.length];
		
					for (int e = 0; e < doubleRates.length; e++) {
						doubleRates[e] = Double.parseDouble(rates[e]);
					}
					
					this.updater = new RandomUpdaterWithRates(model, doubleRates, this.filter);
					
					double[] ratesIdx = ((RandomUpdaterWithRates) this.updater).getRates();
					this.updaterString += Arrays.toString(ratesIdx).replaceAll("\\s+","");
				}						
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
				
				// splitFlag is either +1 or -1
				if (this.vars[i] == idx && this.vars[i + 1] == splitFlag) {
					return true;
				}
			}
			return false;
		}

		public List<String> getVars(LogicalModel m) {
			List<String> lVars = new ArrayList<String>();
			// i += 2 in order to look for only Node idx, skip splitFlag
			for (int i = 0; i < this.vars.length; i += 2) {
				String var = m.getComponents().get(this.vars[i]).getNodeID();
				if (this.vars[i + 1] == 1) {
					var += SplittingType.POSITIVE.toString();
				} else if (this.vars[i + 1] == -1) {
					var += SplittingType.NEGATIVE.toString();
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
					// skips +4, the idx that was updated
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
					
//					 // filter... 
//					NodeInfo node = model.getComponents().get(idx);
//					this.filter.put(node, SplittingType.MERGED);
//					 
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
//			 // get the nodeInfo from the node to be changed
//			NodeInfo node = model.getComponents().get(idx); 
//			SplittingType splt = this.filter.get(node);
//			 
//			// if both +/- are to be removed, do it. 
//			if (splitFlag == 0) {
//				this.filter.remove(node); 
//			} else { 
//				 // if the var is currently MERDED.
//				 // remove the complementing to SplitFlag
//				 if (splt.equals(SplittingType.MERGED)) { 
//					 SplittingType newSplt = (splitFlag == 1) ? 
//							 SplittingType.NEGATIVE : SplittingType.POSITIVE;
//					 this.filter.put(node, newSplt); 
//				 // if just one exists: var[-] or var[+]
//				 } else { 
//					 this.filter.remove(node);
//				 }
//			}
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
				
			// get the nodeInfo from the node to be changed
			
			 NodeInfo node = model.getComponents().get(idx); 
			 SplittingType splt = this.filter.get(node);
			 
			 // if both +/- are to be added, do it. 
//			 if (splitFlag == 0) {
//				 this.filter.put(node, SplittingType.MERGED); 
//			 } else { 
//				 // if only + or - is to be added, check if the complementing exists in the group 
//				 // if so, put as MERGED 
//				 SplittingType compSplt = (splitFlag == 1) ? SplittingType.NEGATIVE 
//						 : SplittingType.POSITIVE; 
//				 if (compSplt.equals(splt)) {
//					 this.filter.put(node, SplittingType.MERGED); 
//					 // neither exists 
//					 } else { 
//						 SplittingType newSplt = (splitFlag == 1) ? SplittingType.POSITIVE
//								 : SplittingType.NEGATIVE;
//						 this.filter.put(node, newSplt); 
//					 } 
//			 }
			 
			return true;
		}

		public void addUpdater(LogicalModelUpdater updater) {
		    updateVarsAndFilter();
		    this.updater = updater;
		    this.updater.setFilter(this.filter);
		    changeUpdaterString();
		}
		
		public void addUpdater(Map<String, Double> rates) {
		    updateVarsAndFilter();
		    if (rates.size() == 0) {
		    	this.updater = new RandomUpdaterWithRates(model,this.filter); 
		    } else {
		    	//  ...      ORDENAÇÃO 
		    	List<Double> tempRates = new ArrayList<Double>();
		    	List<tupleNodeRate> tpNodeRate = new ArrayList<tupleNodeRate>();
		    	for(int i = 0; i < this.vars.length - 1; i+= 2) 
		    		tpNodeRate.add(new tupleNodeRate(this.vars[i], this.vars[i+1]));
	
		    	java.util.Collections.sort(tpNodeRate);
		    	for(tupleNodeRate nodeRate : tpNodeRate) {
		    		
		    		String var = model.getComponents().get(nodeRate.idx).getNodeID();
		    		if (nodeRate.flag == 1) {
						var = var + SplittingType.POSITIVE.toString();
		    		} else if (nodeRate.flag == -1){
		    			var = var + SplittingType.NEGATIVE.toString();
		    		}
		    		tempRates.add(rates.get(var));
		   			tempRates.add(rates.get(var));

		    	double[] newRates = new double[tempRates.size()];
		    	for (int j = 0; j < newRates.length; j++)
		    		newRates[j] = tempRates.get(j);
			
		    	this.updater = new RandomUpdaterWithRates(model, newRates, this.filter); 
		    }
	    	changeUpdaterString();
		    }
		}
		
		public double[] getRates() {
			
			double[] upRates = ((RandomUpdaterWithRates) this.updater).getRates();
			// get Rates to GUI
			System.out.println("this.vars: " + Arrays.toString(this.vars));
			System.out.println("updaterRates: " + Arrays.toString(upRates));
			
			List<Double> tempRates = new ArrayList<Double>();
			
			if (upRates.length == this.vars.length) {
				for (int i=0; i < this.vars.length - 1; i += 2) {
					// if no split or only [+] or [-] exists
					if (this.vars[i + 1] == 0 || (i + 2 < this.vars.length 
							&& this.vars[i] != this.vars[i+2]) || i + 2 == this.vars.length) {
						tempRates.add(upRates[i]);
						// if both exist:
					} else {
						tempRates.add(upRates[i]);
						tempRates.add(upRates[i + 1]);
						}
					}
			// a var has been removed or added by the GUI
			} else {
				List<tupleNodeRate> tpNodeRate = new ArrayList<tupleNodeRate>();
		    	for(int i = 0; i < this.vars.length - 1; i+= 2) 
		    		tpNodeRate.add(new tupleNodeRate(this.vars[i], this.vars[i+1]));
		    	java.util.Collections.sort(tpNodeRate);
		    	
				// a var has been removed
				if (upRates.length > this.vars.length) {
					
					// get relative position
			    	for(tupleNodeRate nodeRate : tpNodeRate) {
			    	}					
				
				// a var has been added
				} else {
					int varIdx = this.vars[this.vars.length - 2];
					int varFlag = this.vars[this.vars.length - 1];
				
				}
			}
			
			double[] rates = new double[tempRates.size()];
			for (int j = 0; j < rates.length; j++) 
				rates[j] = tempRates.get(j);
				
			
			return rates;
		}
		
		public LogicalModelUpdater getUpdater() {
			return this.updater;
		}
		
		public String getUdaterString() {
			return this.updaterString;
		}
		
		public String getUpdaterName() {
			return this.updater.getUpdaterName();
		}
		
		
		private void changeUpdaterString() {
			
		    if (this.updater instanceof RandomUpdaterWrapper){
				this.updaterString = SEPUPDATER + "RU";
		    } else if (this.updater instanceof SynchronousUpdater) {
				this.updaterString = SEPUPDATER + "S";
		    } else if (this.updater instanceof RandomUpdaterWithRates) {
				this.updaterString =  SEPUPDATER + "RN";
				
				double[] ratesIdx = ((RandomUpdaterWithRates) this.updater).getRates();
				this.updaterString += Arrays.toString(ratesIdx).replaceAll("\\s+","");
		    }
		}
		
		
		public void updateVarsAndFilter() {
			
			Map<NodeInfo, SplittingType> newFilter = new HashMap<NodeInfo, SplittingType>();
			
			for (int idx = 0; idx < this.vars.length - 1; idx+=2) {
				NodeInfo node = model.getComponents().get(this.vars[idx]);
				// find Node with var nodeID
			//
		//		System.out.println(node + " " + this.vars[idx] + " " + this.vars[idx + 1 ]);
				if (this.vars[idx+1] == 0) {
					newFilter.put(node, SplittingType.MERGED);
					
				// if only [+] or [-] is present
				} else if ((idx + 2 < this.vars.length 
						&& this.vars[idx] != this.vars[idx+2]) 
						|| idx + 2 == this.vars.length) {
					if (this.vars[idx+1] == 1) {
						newFilter.put(node, SplittingType.POSITIVE);
					} else {
						newFilter.put(node, SplittingType.NEGATIVE);
					}
				} else {
					// we want to merge this.vars
					// except if random rate ?
					if (this.updater instanceof RandomUpdaterWithRates) {
						newFilter.put(node, SplittingType.MERGED);
					} else {
						this.add(this.vars[idx], 0);
						this.remove(this.vars[idx], 1);
						this.remove(this.vars[idx], -1);
						newFilter.put(node, SplittingType.MERGED);
					}
				}
			}
			this.filter = newFilter;
		}

		public RankedClassGroup clone() {
			return new RankedClassGroup(this.vars.clone(), this.updater);
		}
		
		public boolean equals(Object o) {
			RankedClassGroup outPC = (RankedClassGroup) o;
			if (outPC.toString().equals(this.toString()))
				return Arrays.equals(this.vars, outPC.vars);
			return false;
		}
	}
	
	public class tupleNodeRate implements Comparable<tupleNodeRate> {
		
		public final int idx;
		public final int flag;
		
		tupleNodeRate(int idx, int flag) {
			this.idx = idx;
			this.flag = flag;
		}
		
	
		public int compareTo(tupleNodeRate other) {
			   return new Integer(this.idx).compareTo(new Integer(other.idx));
		 }
		   
	}
	

 
}
