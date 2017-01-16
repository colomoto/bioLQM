package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/*
 * This tool will find nodes sharing the same logical function and remove duplicates.
 * These duplicates will be considered as mirror nodes for the first of them and reduced.
 */
public class DuplicateRemover {

	public static LogicalModel removeDuplicateComponents(LogicalModel model) {

		MDDManager ddmanager = model.getMDDManager();

		List<NodeInfo> nodes = model.getNodeOrder();
		int[] functions = model.getLogicalFunctions();
		int[] newFunctions = functions.clone();

		// find and simplify duplicates
		Map<Integer, Integer> m_functions = new HashMap<Integer, Integer>();
		List<Integer> duplicates = new ArrayList<Integer>();
		for (int i=functions.length-1 ; i>=0 ; i--) {
			int n = functions[i];
			if (m_functions.containsKey(n)) {
				// This is a duplicate: turn it into a mirror node
				int ref = m_functions.get( n );
				MDDVariable var = ddmanager.getVariableForKey( nodes.get(ref) );
				if (var.nbval == 2) {
					newFunctions[i] = var.getNode(0, 1);
				} else {
					int[] values = new int[var.nbval];
					for (int k=0 ; k<values.length ; k++) {
						values[k] = k;
					}
					newFunctions[i] = var.getNode(values);
				}
				duplicates.add(i);
			} else {
				m_functions.put(n, i);
			}
		}

		if (duplicates.size() == 0) {
			return model;
		}

		// reduce them
		LogicalModel newModel = new LogicalModelImpl(nodes, ddmanager, newFunctions);
		ModelReducer reducer = new ModelReducer(newModel);
		for (int i: duplicates) {
			reducer.remove(i);
		}

		return reducer.getModel();
	}

}
