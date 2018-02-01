package org.colomoto.biolqm.tool.fixpoints;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * Sort nodes of a LogicalModel according to the new regulators they introduce.
 * This ordering is used to delay the addition of new constraints during stable state search,
 * keeping a "small" MDD as long as possible.
 * 
 * @author Aurelien Naldi
 */
public class StructuralNodeOrderer implements Iterable<Integer> {

	protected final LogicalModel model;

	public StructuralNodeOrderer( LogicalModel model) {
		this.model = model;
	}

	@Override
	public Iterator<Integer> iterator() {
		return new NodeIterator(model);
	}

}

class NodeIterator implements Iterator<Integer> {
	
	int nbgene, nbremain;
	int bestIndex, bestValue;
	
	boolean[][] t_reg;
	int[][] t_newreg;

	public NodeIterator(LogicalModel model) {
		buildAdjTable(model);
	}
	
	private void buildAdjTable(LogicalModel model) {
		List<NodeInfo> nodeOrder = model.getComponents();
		MDDManager ddmanager = model.getMDDManager();
		nbgene = nbremain = nodeOrder.size();
		t_newreg = new int[nbgene][2];
		t_reg = new boolean[nbgene][nbgene];
		bestValue = nbgene+1;
		int[] functions = model.getLogicalFunctions();
		for (int i=0 ; i<nbgene ; i++) {
			Collection<MDDVariable> regulators = getRegulators(ddmanager, functions[i]);
			int cpt = 0;
			boolean[] t_regline = t_reg[i];
			for (MDDVariable var: regulators) {
				int r = ddmanager.getVariableIndex(var);
				t_regline[r] = true;
				cpt++;
			}
			if (!t_reg[i][i]) {
				t_reg[i][i] = true;
				cpt++;
			}
			t_newreg[i][0] = i;
			t_newreg[i][1] = cpt;
			if (cpt < bestValue) {
				bestValue = cpt;
				bestIndex = i;
			}
		}
	}

	@Override
	public Integer next() {
		if (!hasNext()) {
			throw new NoSuchElementException();
		}
		int choice = bestIndex;
		int ret = t_newreg[choice][0];
		bestValue = nbgene+1;
		bestIndex = -1;
		
		boolean[] t_old;
		
		if (choice != -1) {
			// remove the old one
			t_newreg[choice] = t_newreg[--nbremain];
			t_old = t_reg[choice];
			t_reg[choice] = t_reg[nbremain];
			for (int i=0 ; i<t_old.length ; i++) {
				if (t_old[i]) {
					// here is a new regulator to remove
					for (int j=0 ; j<nbremain ; j++) {
						if (t_reg[j][i]) {
							t_reg[j][i] = false;
							t_newreg[j][1]--;
						}
					}
				}
			}

			// update everything here
			for (int i=0 ; i<nbremain ; i++) {
				if (t_newreg[i][1] < bestValue) {
					bestValue = t_newreg[i][1];
					bestIndex = i;
				}
			}
		}
		return ret;
	}

	@Override
	public boolean hasNext() {
		return nbremain > 0;
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

	
	private static Collection<MDDVariable> getRegulators(MDDManager factory, int f) {
		Set<MDDVariable> regulators = new HashSet<MDDVariable>();
		addRegulators(regulators, factory, f);
		return regulators;
	}
	
	private static void addRegulators(Set<MDDVariable> regulators, MDDManager factory, int f) {
		if (factory.isleaf(f)) {
			return;
		}
		
		MDDVariable var = factory.getNodeVariable(f);
		regulators.add(var);
		for (int v=0 ; v<var.nbval ; v++) {
			addRegulators(regulators, factory, factory.getChild(f, v));
		}
	}
}
