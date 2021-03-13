
package org.colomoto.biolqm.tool.simulation.grouping;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;
/**
 * @author Pedro T. Monteiro
 * @author Pedro L. Varela
 */
public class ModelGrouping {

	public static final String SEPVAR = ",";
	public static final String SEPGROUP = "/";
	public static final String SEPCLASS = ":";
	public static final String SEPUPDATER = "$";
	public static final String[] updatersList = 
			new String[] {"Random uniform", "Random non uniform", "Synchronous"};

	protected LogicalModel model;
	private List<RankedClass> pcList;
	

	public ModelGrouping(LogicalModel m) { 
		this.model = m;
		this.pcList = new ArrayList<>();
		

		// Init single class with a single synchronous group
		List<VarInfo> vars = new ArrayList<VarInfo>();
		for (int n = 0; n < this.model.getComponents().size(); n++) {
			if (!this.model.getComponents().get(n).isInput())
				vars.add(new VarInfo(n, 0, model));
		}
		this.pcList.add(new RankedClass(new RankedClassGroup(vars)));
	}

	public ModelGrouping(LogicalModel m, String textFormat) {
		this.model = m;
		this.pcList = new ArrayList<>();
		String[] saPCs = textFormat.split(SEPCLASS);
		for (String sPC : saPCs) {
			this.pcList.add(new RankedClass(m, sPC));
		}
	}

	public ModelGrouping(LogicalModel m, List<RankedClass> pcList) throws Exception {
		this.model = m;
		this.pcList = pcList;
	}
	
	// LogicalModel, Map<Rank, Map<List<GroupVars>,updater>>
	public ModelGrouping(LogicalModel m, Map<Integer, Map<List<VarInfo>, LogicalModelUpdater>> ranks) 
			throws Exception {
		this.model = m;
		this.pcList = new ArrayList<>();
	
		Set<VarInfo> varsTaken = new HashSet<VarInfo>();
		
		for (int i= 0; i < this.model.getComponents().size(); i++) {
			if (!this.model.getComponents().get(i).isInput()) {
				varsTaken.add(new VarInfo(i, 0, model));
				varsTaken.add(new VarInfo(i, -1, model));
				varsTaken.add(new VarInfo(i, 1, model));
			}
		}
		
		for (int rank = 0; rank < ranks.keySet().size(); rank ++) {
			if (!ranks.keySet().contains(rank))
				throw new Exception("Rank order not correct"); 
		}
		
		for (Integer rank : ranks.keySet()) {
			List<RankedClassGroup> rankGroups = new ArrayList<>();
			
			for (List<VarInfo> vars : ranks.get(rank).keySet()) {
				
				for (VarInfo var : vars) {
					
					if (model.getComponents().get(var.idx).isInput())
						throw new Exception("Var is input: " + var.toString()); 
					if (varsTaken.contains(var)) {
						throw new Exception("Duplicate var: " + var.toString()); 
					} else {
						if (var.flag == 0) {
							varsTaken.remove(var);
							varsTaken.remove(new VarInfo(var.idx, -1, model));
							varsTaken.remove(new VarInfo(var.idx, 1, model));
						} else {
							varsTaken.remove(var);
							varsTaken.remove(new VarInfo(var.idx, 0, model));
						}
					}			
				}
				RankedClassGroup newGroup = new RankedClassGroup(vars, ranks.get(rank).get(vars));
				rankGroups.add(newGroup);
			}
			this.pcList.add(new RankedClass(rankGroups));
		}	
		
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
				l += grp.vars.size();
			}
			int[] cur = new int[l];
			int pos = 0;
			for (RankedClassGroup grp : cl.groups) {
				System.arraycopy(grp.vars, 0, cur, pos, grp.vars.size());
				pos += grp.vars.size();
			}
			blocks[idx] = cur;
			idx++;
		}
		return blocks;
	}

	@Override
	public ModelGrouping clone() {
		List<RankedClass> pcNew = new ArrayList<RankedClass>();
		for (RankedClass pc : this.pcList) {
			pcNew.add(pc.clone());
		}
		try {
			return new ModelGrouping(this.model, pcNew);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	// FIXME: hack for EpiLog to pass modified/perturbed models to the PriorityUpdater
	public ModelGrouping cloneWithModel(LogicalModel modifiedModel) throws Exception {
		List<RankedClass> pcNew = new ArrayList<RankedClass>();
		for (RankedClass pc : this.pcList) {
			pcNew.add(pc.clone());
		} 
		return new ModelGrouping(modifiedModel, pcNew);
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
			tmp = this.pcList.get(idxPC).getVars();
		} else { 
			tmp = new ArrayList<>();
			tmp.add(new ArrayList<>());
		}
		return tmp;
	}
	
	public static String[] getUpdatersAvailable() {
		return ModelGrouping.updatersList;
	}

	public void decPriorities(int idxPC, Map<Integer, List<String>> groupsSel, boolean multiGroups) {
		if (!this.isValid(idxPC))
			return;
		
		if (multiGroups) {
			int i = 0;
			for (int group : groupsSel.keySet()) {
				this.decPrioritiesGrp(idxPC, group - i, groupsSel.get(group));
				i ++;
			}
		} else {
			for (int group : groupsSel.keySet()) {
				this.decPriorities(idxPC, group, groupsSel.get(group));
			}
		}
	}
	
	public void decPrioritiesGrp(int idxPC, int idxGrp, List<String> vars) {
		if (!this.pcList.get(idxPC).isValid(idxGrp))
			return;
		
		RankedClassGroup tempGroup = this.pcList.get(idxPC).groups.get(idxGrp).clone();
		RankedClassGroup newGroup = this.pcList.get(idxPC).groups.get(idxGrp).clone();
		
		for (String varMm : tempGroup.getVars()) {
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
						if (vars.contains(varMm)) {
							this.pcList.get(idxPC).remove(idxGrp, idx, splitFlag);
						} else {
							newGroup.remove(idx, splitFlag);	
						}
					}
				}
			}
		}
			
		if (idxPC == this.pcList.size() - 1) {
			this.pcList.add(this.pcList.size(), new RankedClass(newGroup));
		} else {
			this.pcList.get(idxPC + 1).addGrp(this.pcList.get(idxPC + 1).size(),newGroup);
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
						this.pcList.get(idxPC).remove(idxGrp, idx
								
								, splitFlag);
					}
					// If a new class is needed
					if ((idxPC + 1) == this.pcList.size()) {
						List<VarInfo> newvar = new ArrayList<VarInfo>();
						newvar.add(new VarInfo(idx, splitFlag, model));
						this.pcList.add(new RankedClass(new RankedClassGroup(newvar)));
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
							List<VarInfo> newVar = new ArrayList<VarInfo>();
							newVar.add(new VarInfo(idx, splitFlag, model));
							this.pcList.get(idxPC).addGrp(idxGrp + 1, new RankedClassGroup(newVar));
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
							List<VarInfo> newVar = new ArrayList<VarInfo>();
							newVar.add(new VarInfo(idx, splitFlag, model));
							
							// if it was in the top group, create a new one and make it top
							// add variable to it
							this.pcList.get(idxPC).addGrp(0, new RankedClassGroup(newVar));
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
	public void incPriorities(int idxPC, Map<Integer, List<String>> groupsSel, boolean multiGroups) {
		if (!this.isValid(idxPC))
			return;
		
		if (multiGroups) {
			int i = 0;
			for (int group : groupsSel.keySet()) {
				this.incPrioritiesGrp(idxPC, group - i, groupsSel.get(group));
				i ++;
			}
		} else {
			for (int group : groupsSel.keySet()) {
				this.incPriorities(idxPC, group, groupsSel.get(group));
			}
		}
	}
		
	public void incPrioritiesGrp(int idxPC, int idxGrp, List<String> vars) {
		if (!this.pcList.get(idxPC).isValid(idxGrp))
			return;
		
		RankedClassGroup tempGroup = this.pcList.get(idxPC).groups.get(idxGrp).clone();
		RankedClassGroup newGroup = this.pcList.get(idxPC).groups.get(idxGrp).clone();
		
		for (String varMm : tempGroup.getVars()) {
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
						if (vars.contains(varMm)) {
							this.pcList.get(idxPC).remove(idxGrp, idx, splitFlag);
						} else {
							newGroup.remove(idx, splitFlag);	
						}
					}
				}
			}
		}
			
		if (idxPC == 0) {
			this.pcList.add(0, new RankedClass(newGroup));
			idxPC++;
		} else {
			this.pcList.get(idxPC - 1).addGrp(this.pcList.get(idxPC - 1).size(),newGroup);
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
				List<VarInfo> var = new ArrayList<VarInfo>();
				var.add(new VarInfo(newvars[0], newvars[1], model));
				this.pcList.add(0, new RankedClass(new RankedClassGroup(var)));
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
	
	public void switchGroups(int idxPC, int idxGrp, int change) {
		this.pcList.get(idxPC).switchGroups(idxGrp, change);
	}
	

	public void groupExpand(int idxPC) {
		this.pcList.get(idxPC).expand();
	}
	
	public void groupExpand(int idxPC, Map<Integer, List<String>> vars) {
		boolean all = true;
		if (this.pcList.get(idxPC).size() != vars.size()) 
			all = false;
			
		for (int g : vars.keySet()) {
			if (this.pcList.get(idxPC).groups.get(g).size() != vars.get(g).size()) {
				all = false;
				break;
			}	
		}
		if (all) {
			this.groupExpand(idxPC);
		} else {
			// for each group
			for (int idxGrp : vars.keySet()) {
				if (this.pcList.get(idxPC).groups.get(idxGrp).size() != 1) {
					for (String varMm : vars.get(idxGrp)) {
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
								if (this.pcList.get(idxPC).contains(idxGrp, idx, splitFlag)) {
									// remove var from its group	
									this.pcList.get(idxPC).remove(idxGrp, idx, splitFlag);
									// creates new group with var
									
									List<VarInfo> varList = new ArrayList<VarInfo>();
									varList.add(new VarInfo(idx, splitFlag, model));									
									
									RankedClassGroup newGroup = new RankedClassGroup(varList);
									int rankSize = this.pcList.get(idxPC).size();
									// add new group at the end
									this.pcList.get(idxPC).addGrp(rankSize,newGroup);
								}
							}
						}
					}
				}	
			}
		}
		int i = 0;
		for (int idxGrp : vars.keySet()) {
			// If the old group is empty
			if (this.pcList.get(idxPC).groups.get(idxGrp - i).isEmpty()) {
				this.pcList.get(idxPC).groups.remove(idxGrp - i);
				i++;
			}
		}
	}

	public void groupCollapse(int idxPC) {
		this.pcList.get(idxPC).collapse();
	}

	public void groupCollapse(int idxPC, Map<Integer, List<String>> vars) {
		boolean all = true;
		if (this.pcList.get(idxPC).size() != vars.size()) 
			all = false;
			
		for (int g : vars.keySet()) {
			if (this.pcList.get(idxPC).groups.get(g).size() != vars.get(g).size()) {
				all = false;
				break;
			}	
		}
		if (all) {
			this.groupCollapse(idxPC);
		} else {
			RankedClassGroup newGrp = new RankedClassGroup(new ArrayList<VarInfo>());
			this.pcList.get(idxPC).addGrp(this.pcList.get(idxPC).size(), newGrp);
			
			// for each group
			for (int idxGrp : vars.keySet()) {
				for (String varMm : vars.get(idxGrp)) {
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
							if (this.pcList.get(idxPC).contains(idxGrp, idx, splitFlag)) {
								// remove var from its group	
								this.pcList.get(idxPC).remove(idxGrp, idx, splitFlag);
								// add to new group
								this.pcList.get(idxPC).add(this.pcList.get(idxPC).size() - 1, 
										idx, splitFlag);							
							}
						}
					}
				}
			}	
			int i = 0;
			for (int idxGrp : vars.keySet()) {
				// If the old group is empty
				if (this.pcList.get(idxPC).groups.get(idxGrp - i).isEmpty()) {
					this.pcList.get(idxPC).groups.remove(idxGrp - i);
					i++;
				}
			}
		}
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
	
	public Map<String, Double> getRates(int idxPC, int idxGrp, List<String> vars) {
		if (this.isValid(idxPC) && this.pcList.get(idxPC).isValid(idxGrp))
			return this.pcList.get(idxPC).getRates(idxGrp, vars);
		return new HashMap<String, Double>();
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
	
	public void collapseRanks() {
		
		for(int i = 1; i < this.pcList.size(); i++) {
			for(int g = 0; g < this.pcList.get(i).size(); g++) {
				this.pcList.get(0).addGrp(this.pcList.get(0).size(), 
						pcList.get(i).groups.get(g));
			}
		}
		for(int size = pcList.size(); size > 1; size --)
			this.pcList.remove(1);
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
	
	public void acceptGUI() {
		for (int i = 0; i < this.size(); i++) {
			this.pcList.get(i).accept();
		}
	}
	
	@Override
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

		public RankedClass() {
			this.groups = new ArrayList<>();
		}
		
		public RankedClass(RankedClassGroup pcg) {
			this.groups = new ArrayList<>();
			this.groups.add(pcg);
		}
		
		public RankedClass(List<RankedClassGroup> groups) {
			this.groups = groups;
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
			return this.size() == 0 || (this.size() == 1 && 
					this.groups.get(0).isEmpty());
		}

		private boolean contains(int idxGrp, int idxVar, int splitFlag) {
			return this.groups.get(idxGrp).contains(idxVar, splitFlag);
		}
		
		public void switchGroups(int idxGrp, int g) {
			RankedClassGroup grp = this.getGroup(idxGrp);
			this.removeGrp(idxGrp);
			this.addGrp(g, grp);
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

		public List<VarInfo> getGroupValues(int idxGrp) {
			if (idxGrp >= 0 && idxGrp < this.size()) {
				return this.groups.get(idxGrp).array();
			}
			return null;
		}
		
		public RankedClassGroup getGroup(int idxGrp) {
			if (!this.isValid(idxGrp))
				return null;
			return this.groups.get(idxGrp);
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
		
		public Map<String, Double> getRates(int idxGrp, List<String> vars) {
			if (idxGrp >= 0 && idxGrp < this.size()) 
				return this.groups.get(idxGrp).getRates(vars);
			return new HashMap<String, Double>();
		}
			
				
		public List<List<String>> getVars() {
			List<List<String>> lVars = new ArrayList<List<String>>();
			for (RankedClassGroup pcg : this.groups) {
				lVars.add(pcg.getVars());
			}
			return lVars;
		}


		public void collapse() {
			for (int g = this.size() - 1; g > 0; g--) {
				List<VarInfo> vars = this.getGroupValues(g);
				for (int i = 0; i < vars.size(); i ++) {
					this.groups.get(0).add(vars.get(i).idx, vars.get(i).flag);
				}
				this.groups.remove(g);
			}
		}

		public void expand() {
			List<RankedClassGroup> lPCGs = new ArrayList<RankedClassGroup>();
			for (RankedClassGroup pcg : this.groups) {
				List<VarInfo> vars = pcg.array();
				for (int i = 0; i < vars.size(); i ++) {
					List<VarInfo> varList = new ArrayList<VarInfo>();
					varList.add(new VarInfo(vars.get(i).idx,vars.get(i).flag, model));
					RankedClassGroup pcgNew = new RankedClassGroup(varList);
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
					if (this.groups.get(g).isEmpty())
						this.removeGrp(g);
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
				this.groups.get(g).acceptVars();
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
				for (int i = 0; i < pcg.vars.size(); i ++) {
					if (!sG.isEmpty())
						sG += SEPVAR;
					sG += pcg.vars.get(i).toString();
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

		private List<VarInfo> vars;
		private LogicalModelUpdater updater;
		List<VarInfo> varsInfo = new ArrayList<VarInfo>();

		
		public RankedClassGroup(List<VarInfo> varList) {
			this.setVars(varList);
			this.addUpdater(new SynchronousUpdater(model));
		}
		
		public RankedClassGroup(List<VarInfo> vars, LogicalModelUpdater updater) {
			this.setVars(vars);
			this.addUpdater(updater);
		}
		
		public RankedClassGroup(LogicalModel m, String textFormat) {
			String[] up = textFormat.split("\\" + SEPUPDATER);
			String[] saVars = up[0].split(SEPVAR);
			List<VarInfo> varsInfo = new ArrayList<VarInfo>();

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
				for (int idx = 0; idx < m.getComponents().size(); idx++) {
					NodeInfo node = m.getComponents().get(idx);
					// find Node with var nodeID
					if (node.getNodeID().equals(var)) {
						if (node.isInput()) {
							continue vars;
						} else {
							varsInfo.add(new VarInfo(idx, split, model));
							break;
						}
					}
				}
			}
			this.setVars(varsInfo);
			
			// get updater
			if (up.length == 1) {
				// default, Synchronous
				this.addUpdater(new SynchronousUpdater(model));
			} else if (up.length == 2) {
				// either $S, $RN or $RU
				if (up[1].equals("S")) {
					this.addUpdater(new SynchronousUpdater(model));
				} else if (up[1].equals("RU")) {
					MultipleSuccessorsUpdater MultiUpdater = new AsynchronousUpdater(model);
					this.addUpdater(new RandomUpdaterWrapper(MultiUpdater));
		
				} else if (up[1].equals("C")) {
					// ?? 
				} else if (up[1].equals("BS")) {
					// ?? 
				} else if (up[1].equals("A")) {
					// ?? 
				} else {
					// $RN[0.3,0.5,...], get rates
					String[] rates = up[1].substring(3, up[1].length() - 1).split(",");
					double[] doubleRates = new double[rates.length];
					for (int e = 0; e < doubleRates.length; e++) {
						doubleRates[e] = Double.parseDouble(rates[e]);
					}
			
					this.updater = new RandomUpdaterWithRates(model, doubleRates, this.getFilter());
				}						
			}
		}

		public List<VarInfo> array() {
			return this.vars;
		}

		public boolean isEmpty() {
			return (this.vars == null || this.vars.size() < 1);
		}

		public int size() {
			return this.vars.size();
		}

		private boolean contains(int idx, int splitFlag) {
			for (int i = 0; i < this.vars.size(); i++) {
				
				if(this.vars.get(i).idx == idx && this.vars.get(i).flag == splitFlag)
					return true;
			}
			return false;
		}

		public List<String> getVars() {
			
			List<String> lVars = new ArrayList<String>();
			// i += 2 in order to look for only Node idx, skip splitFlag
			for (int i = 0; i < this.vars.size(); i ++) {
				lVars.add(this.vars.get(i).toString());
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
			
			for (int i = 0; i < this.vars.size(); i ++) {
				if (this.vars.get(i).idx == idx)	{
					this.vars.remove(i);
					this.vars.add(new VarInfo(idx, -1, model));
					this.vars.add(new VarInfo(idx, 1, model));
					this.setVars(this.vars);
					break;
				}	
			}

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

			for (int i = 0; i < this.vars.size(); i ++) {
				if (this.vars.get(i).idx == idx) {
					this.vars.get(i).setFlag(0);
					this.setVars(this.vars);
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

			for (int i = 0; i < this.vars.size(); i ++) {
				if (this.vars.get(i).idx == idx && this.vars.get(i).flag == splitFlag) {
					this.vars.remove(i);
					this.setVars(this.vars);
					break;
				}
			}
			return true;
		}

		public boolean add(int idx, int splitFlag) {
			if (this.contains(idx, splitFlag))
				return false;

			this.vars.add(new VarInfo(idx, splitFlag, model));
			this.setVars(this.vars);
			return true;
		}

		public void addUpdater(LogicalModelUpdater updater) {
		    this.updater = updater;
		    this.updater.setFilter(this.getFilter());
		}
		
		public void addUpdater(Map<String, Double> rates) {
		    if (rates.size() == 0) {
		    	this.updater = new RandomUpdaterWithRates(model, this.getFilter()); 
		    } else {
		    	List<Double> tempRates = new ArrayList<Double>();

		    	for(int e = 0; e < this.vars.size(); e++) {
		    		
		    		VarInfo nodeRate = this.vars.get(e);
		    		String var = model.getComponents().get(nodeRate.idx).getNodeID();   		
		    			
			    		// if split, and both [+], [-] are present 
		    			if (e + 1 < this.vars.size() 
		    					&& this.vars.get(e+1).idx == nodeRate.idx) {
		    				String deepVar = var;
 
			    			var = deepVar + SplittingType.NEGATIVE.toString();
				   			tempRates.add(rates.get(var));
				   			
							var = deepVar + SplittingType.POSITIVE.toString();
				   			tempRates.add(rates.get(var));
				   			
				   			e ++; 				
		    			} else {
		    				// verificar se 
		    				if (nodeRate.flag == 1) {
		    					var = var + SplittingType.POSITIVE.toString();

		    				} else if (nodeRate.flag == -1){
		    					var = var + SplittingType.NEGATIVE.toString();
		    	 
		    				}
		    				
		    				tempRates.add(rates.get(var));
		    				tempRates.add(rates.get(var));
		    			}
		    		
		    	}
		    	double[] newRates = new double[tempRates.size()];
		    	for (int j = 0; j < newRates.length; j++)
		    		newRates[j] = tempRates.get(j);
		    	this.updater = new RandomUpdaterWithRates(model, newRates, this.getFilter()); 
		    }
		}
		
		public Map<String, Double> getRates(List<String> vars) {
			Map<String, Double> rates = this.getRates();
			
			Set<String> toRemove = new HashSet<String>();
			for (String node: rates.keySet()) {
				if (!vars.contains(node)) 
					toRemove.add(node);
			}
			for (String node : toRemove)
				rates.remove(node);
			
			return rates;
			
		}
		
		
		public Map<String, Double> getRates() {
			
			// old Rates and oldFilter (might have changed)
			double[] upRates = ((RandomUpdaterWithRates) this.updater).getRates();
			SplittingType[] splt = ((RandomUpdaterWithRates) this.updater).getFilter();
			Map<String, Double> nodeRates = new HashMap<String, Double>();
			
			
	    	for(int idx = 0, rate = 0; idx < splt.length
	    			&& rate < upRates.length - 1; idx ++) {
	    		String var = model.getComponents().get(idx).getNodeID();

	    		if (splt[idx] != null) {
	    			if (splt[idx] == SplittingType.MERGED) {
	    				// not split, rate[-] == rate[+]
	    				nodeRates.put(var, upRates[rate]);
		    			// split, rate[-] != rate[+]
		    	 		nodeRates.put((var + SplittingType.NEGATIVE.toString()), upRates[rate]);
		    	 		nodeRates.put((var + SplittingType.POSITIVE.toString()), upRates[rate+1]);	
	    			} else if (splt[idx] == SplittingType.POSITIVE) {
	    				nodeRates.put((var + SplittingType.POSITIVE.toString()), upRates[rate+1]);
	    			} else if (splt[idx] == SplittingType.NEGATIVE) {
	    				nodeRates.put((var + SplittingType.NEGATIVE.toString()), upRates[rate]);
	    			}
	    			rate += 2;
	    		}
	    	}
	    	return nodeRates;
		}			
		
		public LogicalModelUpdater getUpdater() {
			return this.updater;
		}
		
		public String getUdaterString() {
			if (this.updater instanceof RandomUpdaterWrapper){
				return SEPUPDATER + "RU";
			} else if (this.updater instanceof SynchronousUpdater) {
				return "";
			} else if (this.updater instanceof RandomUpdaterWithRates) {
				String updater = SEPUPDATER + "RN";
				double[] ratesIdx = ((RandomUpdaterWithRates) this.updater).getRates();
				return updater += Arrays.toString(ratesIdx).replaceAll("\\s+","");
			}
			return "";
		}
		
		public String getUpdaterName() {
			return this.updater.getUpdaterName();
		}
		
		private Map<NodeInfo, SplittingType> getFilter() {
			Map<NodeInfo, SplittingType> filter = new HashMap<NodeInfo, SplittingType>();


	    	for(int e = 0; e < this.vars.size(); e++) {
	    		VarInfo var = this.vars.get(e);
	    		NodeInfo node = model.getComponents().get(var.idx);
				
				// find Node with var nodeID
				if (var.flag == 0) {
					filter.put(node, SplittingType.MERGED);
					
				// if only [+] or [-] is present
				} else if (e == this.vars.size() - 1 ||
						(e < this.vars.size() - 1 && var.idx != this.vars.get(e+1).idx)) {
					if (var.flag == 1) {
						filter.put(node, SplittingType.POSITIVE);
					} else {
						filter.put(node, SplittingType.NEGATIVE);
					}
				} else {
					filter.put(node, SplittingType.MERGED);
					e ++;
				}
			}
			return filter;
		}
				
		public void acceptVars() {
						
			List<VarInfo> tempVars = new ArrayList<VarInfo>();
			tempVars.addAll(this.vars);

			
			for (int i = 0; i < tempVars.size(); i++) {
			
				if ((i < tempVars.size() - 2) && !(tempVars.get(i).flag == 0) 
						&& tempVars.get(i).idx == tempVars.get(i+1).idx) {
					// we want to merge this.vars
					// except if random rate ?
					if (!(this.updater instanceof RandomUpdaterWithRates)) {
						this.remove(tempVars.get(i).idx,-1);
						this.remove(tempVars.get(i).idx,+1);
						this.add(tempVars.get(i).idx,0);
					}
				}
			}
		}
		
		public void setVars(List<VarInfo> vars) {
			this.vars = vars;
			if (!this.vars.isEmpty())
				java.util.Collections.sort(this.vars);
		}

		public RankedClassGroup clone() {
			return new RankedClassGroup(new ArrayList<VarInfo>(this.vars), this.updater);
		}
		
		public boolean equals(Object o) {
			RankedClassGroup outPC = (RankedClassGroup) o;
			if (outPC.toString().equals(this.toString()))
				return this.vars.equals(outPC.vars);
			return false;
		}
	}
	
	public static class VarInfo implements Comparable<VarInfo> {
		
		public int idx;
		public int flag;
		public LogicalModel model;
		
		public VarInfo(int idx, int flag, LogicalModel model) {
			this.model = model;
			this.idx = idx;
			this.flag = flag;
		}
		
		public void setFlag(int flag) {
			if (flag > -2 && flag < 2)
				this.flag = flag;
		}
		
		public String toString() {
			
			String var = model.getComponents().get(this.idx).getNodeID();
			if (this.flag == -1) {
				var += SplittingType.NEGATIVE.toString();
			} else if (this.flag == 1) {
				var += SplittingType.POSITIVE.toString();
			}
		
			return var;
		}
		

		public int compareTo(VarInfo other) {
			if (this.idx != other.idx) {
			   return ((Integer) (this.idx)).compareTo((Integer) other.idx);
			} else {
				return ((Integer) (this.flag)).compareTo((Integer) other.flag);
			}
		 }
		
		public VarInfo clone() {
			return new VarInfo(this.idx, this.flag, this.model);
			
		}
   
	}
}
