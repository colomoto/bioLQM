package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseExporter;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.List;
import java.util.Vector;



/**
 * Export a regulatory graph to petri net (shared methods).
 *
 *<p> translating a regulatory graph to a petri net is done as follow:
 * <ul>
 *  <li>each node will be represented by two places, a negative one and a positive one.
 *      Markers in the positive place represent it's level. if it is not at it's maximum
 *      missing marker(s) will be in it's negative place: the number of markers in the petri net is constant</li>
 *  <li>each logical parameter will be represented by transition(s) with "test" arcs to
 *      non-modified places and "normal" arcs to the positive and negative place of the modified place.</li>
 * </ul>
 *
 * with some simplifications:
 * <ul>
 *  <li>work on the tree representation of logical parameters and use ranges instead of exact values as precondition of transitions</li>
 *  <li>"input" nodes are specials: no transition will affect them and their basal value will be used as initial markup</li>
 *  <li>autoregulation can trigger some cases where a transition can't be fired, these are not created</li>
 * </ul>
 *
 *<p>references:
 *<ul>
 *  <li>Simao, E., Remy, E., Thieffry, D. and Chaouiya, C.: Qualitative modelling of
 *      Regulated Metabolic Pathways: Application to the Tryptophan biosynthesis in E. Coli.</li>
 *  <li>Chaouiya, C., Remy, E. and Thieffry, D.: Petri Net Modelling of Biological Regulatory
 *      Networks</li>
 *</ul>
 */
public abstract class AbstractPNEncoder extends BaseExporter {

	public final List<NodeInfo> nodeOrder;
	public final MDDManager ddmanager;
	public final int[] functions;
	public final int len;

    private int[][] t_priorities = null;
    private byte[] initialstate = null;


    public AbstractPNEncoder( LogicalModel model) {
		super(model);
		this.nodeOrder = model.getComponents();
		this.ddmanager = model.getMDDManager();
		this.functions = model.getLogicalFunctions();
		
		this.len = nodeOrder.size();
		
		// TODO: support output nodes
	}
	
	// TODO: make extension data available
	
    /**
     * extract transitions from a tree view of logical parameters.
     *
     * @param v_result
     * @param nodeIndex index of the considered node (in the regulatory graph)
     * @param v_node all nodes
     * @param len number of nodes in the original graph
     */
    private void browse(List v_result, MDDManager ddmanager, int f, int[][] t_priorities, int nodeIndex, List<NodeInfo> v_node, int len) {
        if (ddmanager.isleaf(f)) {
            TransitionData td = new TransitionData();
            td.value = f;
            td.maxValue = v_node.get(nodeIndex).getMax();
            td.nodeIndex = nodeIndex;
            td.t_cst = null;
            if (t_priorities != null) {
				td.increasePriority = t_priorities[nodeIndex][0];
				td.decreasePriority = t_priorities[nodeIndex][1];
			}
            v_result.add(td);
        } else {
            int[][] t_cst = new int[len][3];
            for (int i=0 ; i<t_cst.length ; i++) {
                t_cst[i][0] = -1;
            }
            browse(v_result, t_cst, 0, ddmanager, f, t_priorities, nodeIndex, v_node);
        }
    }

    private void browse(List v_result, int[][] t_cst, int level, MDDManager ddmanager, int f, int[][] t_priorities, int nodeIndex, List<NodeInfo> v_node) {
        if (ddmanager.isleaf(f)) {
            TransitionData td = new TransitionData();
            td.value = f;
            td.maxValue = v_node.get(nodeIndex).getMax();
            td.nodeIndex = nodeIndex;
            if (t_priorities != null) {
				td.increasePriority = t_priorities[nodeIndex][0];
				td.decreasePriority = t_priorities[nodeIndex][1];
			}
            td.t_cst = new int[t_cst.length][3];
            int ti = 0;
            for (int i=0 ; i<t_cst.length ; i++) {
                int index = t_cst[i][0];
                if (index == -1) {
                    break;
                }
                if (index == nodeIndex) {
                    td.minValue = t_cst[i][1];
                    td.maxValue = t_cst[i][2];
                } else {
                    td.t_cst[ti][0] = index;
                    td.t_cst[ti][1] = t_cst[i][1];
                    td.t_cst[ti][2] = v_node.get(index).getMax() - t_cst[i][2];
                    if (td.t_cst[ti][1] > 0 || td.t_cst[ti][2] > 0) {
                        ti++;
                    }
                }
            }
            if (ti == 0) {
                td.t_cst = null;
            } else {
                td.t_cst[ti][0] = -1;
            }
            v_result.add(td);
            return;
        }

        // specify on which node constraints are added
        MDDVariable var = ddmanager.getNodeVariable(f);
        t_cst[level][0] = ddmanager.getVariableIndex( var);
        for (int i=0 ; i<var.nbval; i++) {
            int next = ddmanager.getChild(f, i);
            int j=i+1;
            while(j<var.nbval) {
                if (ddmanager.getChild(f, j) == next) {
                    j++;
                } else {
                    break;
                }
            }
            j--;
            t_cst[level][1] = i;
            t_cst[level][2] = j;
            browse(v_result, t_cst, level+1, ddmanager, next, t_priorities, nodeIndex, v_node);
            i = j;
        }
        // "forget" added constraints
        t_cst[level][0] = -1;
    }

    
	/**
	 * prepare the PN export:
	 *   - read/set initial markup
	 *   - build the set of transitions
	 *
	 * @param t_transition
	 * @return the initial markup
	 */
    private byte[][] prepareExport( List[] t_transition) {
		// get the selected initial state
    	if (initialstate == null) {
    		initialstate = new byte[len];
    	}

		// keep that for later use of priority classes
		int[][] t_priorities = null;
		byte[][] t_markup = new byte[len][2];
        for (int i=0 ; i<len ; i++) {
            int f = functions[i];
            NodeInfo vertex = nodeOrder.get(i);

//            if (manager.getIncomingEdges(vertex).size() == 0) {
//                // input node: no regulator, use basal value as initial markup ??
//                t_markup[i][0] = vertex.getBaseValue();
//                t_markup[i][1] = (byte)(vertex.getMaxValue() - vertex.getBaseValue());
//            } else {
                // normal node, initial markup = 0
                t_markup[i][0] = (byte)initialstate[i];
                t_markup[i][1] = (byte)(vertex.getMax()-initialstate[i]);
                Vector v_transition = new Vector();
                t_transition[i] = v_transition;
                browse(v_transition, ddmanager, f, t_priorities, i, nodeOrder, len);
//            }
        }
		return t_markup;
    }
    
    abstract protected void doExport( String netName, List<NodeInfo> nodes, List[] t_transition, byte[][] t_markup, OutputStreamWriter out) throws IOException;
    
    public void export() throws IOException {
    	
    	// start with the common parts
        List[] t_transition = new List[len];
        byte[][] t_markup = prepareExport( t_transition);

        // TODO: add support for output nodes?
        OutputStreamWriter writer = streams.writer();
    	doExport( "defaultName", nodeOrder, t_transition, t_markup, writer);
    	writer.close();
    }

    public void setInitialState(byte[] init) {
        this.initialstate = init;
    }

    public byte[] getInitialstate() {
        return initialstate;
    }

}

class TransitionData {
    /** target value of this transition */
    public int value;
    
    /** index of the concerned node */
    public int nodeIndex;

    /** minvalue for the concerned node (0 unless an autoregulation is present) */
    public int minValue;
    /** maxvalue for the concerned node (same as node's maxvalue unless an autoregulation is present) */
    public int maxValue;
    
    /** priority when decreasing */
    public int decreasePriority = 0;
    /** priority when increasing */
    public int increasePriority = 0;
    
    /** constraints of this transition: each row express range constraint for one of the nodes
     * and contains 3 values:
     *  <ul>
     *      <li>index of the node (or -1 after the last constraint)</li>
     *      <li>bottom and top limit of the range (top limit is pre-processed: maxvalue - realLimit)</li>
     *  </ul>
     */
    public int[][] t_cst;
    
}
