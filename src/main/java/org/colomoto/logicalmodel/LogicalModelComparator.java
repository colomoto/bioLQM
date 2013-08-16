package org.colomoto.logicalmodel;

import java.util.List;

import org.colomoto.mddlib.MDDComparator;
import org.colomoto.mddlib.MDDComparatorFactory;
import org.colomoto.mddlib.MDDManager;

/**
 * Test if two logical models share the same variables and equivalent functions
 * 
 * @author Aurelien Naldi
 */
public class LogicalModelComparator {

	/**
	 * Compare two logical models.
	 * This will test if they have the same core variables and functions.
	 * Extra variables are not tested (yet).
	 * 
	 * @param m1
	 * @param m2
	 * @return
	 */
	public static boolean compare(LogicalModel m1, LogicalModel m2) {
		
		List<NodeInfo> nodes1 = m1.getNodeOrder();
		List<NodeInfo> nodes2 = m2.getNodeOrder();
		int n = nodes1.size();
		if (n != nodes2.size()) {
			return false;
		}
		
		int i=0;
		int[] nodeMap = new int[n];
		for (NodeInfo ni: nodes1) {
			int j = nodes2.indexOf(ni);
			if (j < 0) {
				return false;
			}
			nodeMap[i] = j;
			i++;
		}
		
		MDDComparator comparator = MDDComparatorFactory.getComparator(m1.getMDDManager(), m2.getMDDManager());
		int[] functions1 = m1.getLogicalFunctions();
		int[] functions2 = m2.getLogicalFunctions();
		for (i=0 ; i<n ; i++) {
			if (!comparator.similar(functions1[i], functions2[ nodeMap[i] ])) {
				return false;
			}
		}
		
		return true;
	}
}
