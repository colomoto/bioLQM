package org.colomoto.biolqm.helper.inferinteraction;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

/**
 * Extract detailed interactions from a logical model.
 * If you only need lists of regulators, you can use {@link ConnectivityMatrix},
 * this searcher will also provide thresholds and signs.
 * 
 * <p>Basic idea: for each function, unfold the list of leaves and
 * use this "full" list to look for the effects of each regulator.
 * 
 * <p>Usage: it will be needed to infer interactions for the GINML export
 * and correct signs for the SBML export
 * 
 * @author Duncan Berenguier
 * @author Aurelien Naldi
 */
public class InteractionSearcher {

	protected static final byte FUNC_NON = 1;
	protected static final byte FUNC_POSITIVE = 2;
	protected static final byte FUNC_NEGATIVE = 3;
	protected static final byte FUNC_DUAL = 4;

	private List<PathItem> currentPath;
	private List<ReportItem> currentSource;
	
	private final LogicalModel model;
	private final ConnectivityMatrix matrix;

	private final MDDManager ddmanager;
	private final MDDVariable[] variables;

	
	public InteractionSearcher(LogicalModel model) {

		this.model = model;
		this.ddmanager = model.getMDDManager();
		this.variables = ddmanager.getAllVariables();
		this.matrix = new ConnectivityMatrix(model);
	}
	
	
	public void run() {
		
		MDDVariable[] variables = ddmanager.getAllVariables();
		int[] functions = model.getLogicalFunctions();
		
		// lookup incoming interactions for each node
		for (int n=0 ; n<functions.length ; n++) {
			int omdd = functions[n];
			int[] regulators = matrix.getRegulators(n, false);
			
			// compute all subtree sizes
			int nbleaves = 1;
			int[] subtree_sizes = new int[regulators.length];
			for (int i=regulators.length-1 ; i>=0 ; i--) {
				subtree_sizes[i] = nbleaves;
				nbleaves *= variables[ regulators[i] ].nbval;
			}
			int[] allleaves = new int[nbleaves];

			unfoldMDD(omdd, regulators, subtree_sizes, allleaves);
			

			/* ******************** DEBUG ******************  */
			// TODO: remove debug output
			for (int r: regulators) {
				System.out.print(r+" ");
			}
			System.out.print(" --> ");
			
			for (int l: allleaves) {
				if (l < 0) {
					System.out.print("* ");
				} else {
					System.out.print(l+" ");
				}
			}
			System.out.println();
			/* ****************** END DEBUG ****************  */

			
			for (int r: regulators) {
				
				int srcnbval = variables[r].nbval;
				currentSource = new ArrayList<ReportItem>();
				byte functionality = computeFunctionality(srcnbval, r, allleaves, subtree_sizes, regulators); //Compute its functionality
				
				System.out.println("Found ("+r+","+n+") --> "+functionality);
			}
		}
	}

	/**
	 * Browse an ROMDD to extract all "real" leaves, corresponding to a fully uncompressed DD.
	 * 
	 * <p>For example, "A and (B or not C)" has the leaves:  0 0 0 0 1 0 1 1
	 * 
	 * @param omdd the current OMDDNode to scan. Should be the root at the first call.
	 * @param regulators
	 * @param subtree_sizes
	 * @param allLeaves array to store the result
	 * 
	 * @return the explicit list of leaves
	 */
	private void unfoldMDD(int omdd, int[] regulators, int[] subtree_sizes, int[] allLeaves) {

		PathSearcher searcher = new PathSearcher(ddmanager);
		searcher.setNode(omdd);
		int[] path = searcher.getPath();
		int[] simplePath = new int[regulators.length];
		
		for (int leaf: searcher) {
			if (leaf == 0) {
				continue;
			}
			
			// transform into a simple path
			for (int i=0 ; i<regulators.length ; i++) {
				simplePath[i] = path[ regulators[i] ];
			}
			
			// recursively enumerate all matching full paths and set the leaves
			unfoldLeaves(leaf, simplePath, subtree_sizes, allLeaves, regulators,  0, 0);
		}
		
	}
	
	private void unfoldLeaves(int leaf, int[] simplePath, int[] subtree_sizes, int[] allLeaves, int[] regulators, int curReg, int idx) {

		while (curReg < simplePath.length) {
			int v = simplePath[curReg];
			int size = subtree_sizes[curReg];
			
			if (v < 0) {
				// recursive call and return
				int nbval = variables[ regulators[curReg] ].nbval;
				curReg++;
				for (int k=0 ; k<nbval ; k++) {
					// recursive call for each value
					unfoldLeaves(leaf, simplePath, subtree_sizes, allLeaves, regulators, curReg, idx);
					idx += size;
				}
				return;
			}
			
			idx += v*size;
			curReg++;
		}

		allLeaves[idx] = leaf;
	}
	
	/**
	 * Compute the functionality of the 'node_index'-nth node in the omdd represented by 'leafs'.
	 * 
	 * @param count_childs the count of child above 'node_index'
	 * @param node_index the node to consider
	 * @param leafs a table of all the leafs of the complete omdd tree.
	 * @param subtree_size_t the size of the subtree
	 * @param small_node_order the node order in the subtree
	 * @return
	 */
	private byte computeFunctionality(int count_childs, int node_index, int[] leafs, int[] subtree_size_t, int[] small_node_order) {
		int size_of_subtree = subtree_size_t[node_index];
		
		ReportItem ri = null;
		byte res = FUNC_NON;
		boolean containsPositive = false, containsNegative = false;
		
		int index = 0;
		while (index+size_of_subtree < leafs.length) {
			for (int i_childs = 0; i_childs < count_childs - 1; i_childs++) {
				for (int i_subtree = 0; i_subtree < size_of_subtree; i_subtree++) {
					int low = leafs[index];
					int high = leafs[index+size_of_subtree];
					
					ri = new ReportItem();
					ri.targetValue_low = (byte) low;
					ri.targetValue_high = (byte) high;
					currentPath = new LinkedList<PathItem>();
					log_path(index, node_index, ri, subtree_size_t, small_node_order);
					
					if (low < high) {
						containsPositive = true;
						res = FUNC_POSITIVE;
					} else if (low > high) {
						containsNegative = true;
						res = FUNC_NEGATIVE;
					} else {
						res = FUNC_NON;
					}
					index++;
					ri.sign = res;
					ri.path = currentPath;
					currentSource.add(ri);
				}
			}
			index+=size_of_subtree;
		}
		if (containsNegative) {
	        if (containsPositive) {
	            return FUNC_DUAL;
	        }
	        return FUNC_NEGATIVE;
		}
		if (containsPositive) {
		    return FUNC_POSITIVE;
		}
		return FUNC_NON;
	}

	/**
	 * Log the path corresponding to the 'index'-nth leaf.
	 *
	 * @param index the leaf to consider.
	 * @param node_index the index of the source node of the current interaction.
	 * @param subtree_size_t a table of all the subtree size.
	 * @param small_node_order the node order in the subtree.
	 */
	private void log_path(int index, int node_index, ReportItem ri, int[] subtree_size_t, int[] small_node_order) {
		for (int k = 0; k < small_node_order.length; k++) {
			int v = small_node_order[k];
			byte count = (byte) (index/subtree_size_t[k]%(variables[v].nbval));
			if (k != node_index) {
				PathItem pi = new PathItem();
				pi.targetValue_low = count;
				pi.vertex = v;
				currentPath.add(pi);
			} else {
				ri.sourceValue_low = count;
			}
		}
	}

}


class SourceItem {
	List<ReportItem> reportItems = new LinkedList<ReportItem>();
	int source;
	byte sign;
}

/**
 * targetValue_low : value of the target for a low source
 * targetValue_high : value of the target for a high source
 * sourceValue_low : value of the low source, the high value is the low+1
 * sign : represent the sign of the interaction (-1, 0, +1)
 */
class ReportItem {
	byte targetValue_low, targetValue_high, sourceValue_low, sign;
	List<PathItem> path;
}

class PathItem {
	byte targetValue_low, targetValue_high = -1;
	int vertex;
}
