package org.colomoto.logicalmodel.tool.booleanize;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.internal.MDDStoreImpl;

import java.util.*;

/**
 * [WIP] Construct a Boolean version of a multi-valued model.
 * If the model has no multi-valued component, it will be preserved.
 * Otherwise, each multivalued component will be replaced by multiple Boolean components.
 *
 * It implements the Van Ham Boolean mapping to ensure the construction of a compatible model.
 *
 * @author Aurelien Naldi
 */
public class Booleanizer {

    public static LogicalModel booleanize( LogicalModel ori) {

        if ( ori.isBoolean()) {
            return ori;
        }

        Booleanizer bool = new Booleanizer( ori );
        return bool.getModel();
    }

    private final MDDManager ddm, newDDM;
    private final List<NodeInfo> core, extra, newCore, newExtra;
    private final int[] coreFunctions, extraFunctions, newCoreFunctions, newExtraFunctions;
    private final Map<NodeInfo, List<NodeInfo>> mv2bool = new HashMap<NodeInfo, List<NodeInfo>>();


    public Booleanizer(LogicalModel ori) {
        this.ddm = ori.getMDDManager();
        this.core = ori.getNodeOrder();
        this.extra = ori.getExtraComponents();
        this.coreFunctions = ori.getLogicalFunctions();
        this.extraFunctions = ori.getExtraLogicalFunctions();

        this.newCore  = addComponents( core, mv2bool);
        this.newExtra = addComponents( extra, mv2bool);

        this.newDDM = getBoolManager(ddm, mv2bool);
        this.newCoreFunctions = new int[ newCore.size() ];
        this.newExtraFunctions = new int[ newExtra.size() ];

        transformFunctions(core, coreFunctions, newCoreFunctions);
        transformFunctions(extra, extraFunctions, newExtraFunctions);
    }

    private MDDManager getBoolManager( MDDManager ddm, Map<NodeInfo, List<NodeInfo>> mv2bool) {

        // FIXME: construct a clean list of variables

        return new MDDStoreImpl( newCore, ddm.getLeafCount() );
    }

    private List<NodeInfo> addComponents( List<NodeInfo> nodes, Map<NodeInfo, List<NodeInfo>> mv2bool) {

        List<NodeInfo> newComponents = new ArrayList<NodeInfo>();
        for (NodeInfo ni: nodes) {
            int max = ni.getMax();
            if (max > 1) {
                String name = ni.getNodeID();
                List<NodeInfo> mapped = new ArrayList<NodeInfo>(max);
                for (int i=1 ; i<= max ; i++) {
                    NodeInfo bnode = new NodeInfo(name+"_b"+i);
                    mapped.add( bnode);
                    newComponents.add(bnode);
                }
                mv2bool.put(ni, mapped);
            } else {
                newComponents.add(ni);
            }
        }

        return newComponents;
    }


    private void transformFunctions( List<NodeInfo> nodes, int[] srcFunctions, int[] targetFunctions) {

        int s = 0;
        int t = 0;
        for (NodeInfo ni: nodes) {
            List<NodeInfo> bnodes = mv2bool.get(ni);
            int f = srcFunctions[s++];

            if (bnodes == null) {
                targetFunctions[t++] = transform(f, 1);
            } else {
                for (int i=0 ; i<bnodes.size() ; i++) {
                    targetFunctions[t++] = transform(f, i+1);
                }
            }
        }
    }


    /**
     * Take a Boolean function and transfer it into the new MDDManager,
     * replacing the multi-valued regulators if any.
     *
     * @param f MDD in the original MDDManager
     * @return a new MDD in the Boolean MDDManager
     */
    public int transform(int f, int v) {

        if (ddm.isleaf(f)) {
            if (f >= v) {
                return 1;
            }

            return 0;
        }

        MDDVariable var = ddm.getNodeVariable(f);
        if (var.nbval == 2) {
            MDDVariable newVar = newDDM.getVariableForKey( var.key );
            if (newVar == null) {
                throw new RuntimeException("No matching variable during Boolean conversion");
            }

            int c0 = transform( ddm.getChild(f, 0), v );
            int c1 = transform( ddm.getChild(f, 1), v );
            return newVar.getNodeFree(c0, c1);
        }

        // The current variable is multivalued: replace it by the Boolean placeholders
        MDDVariable[] newVars = null; // FIXME: retrieve replacement variables
        int[] values = ddm.getChildren(f);
        int[] newValues = new int[ values.length ];
        for (int i=0 ; i<values.length ; i++) {
            newValues[i] = transform(values[i], v);
        }
        int cur = newValues[ newValues.length-1 ];
        for (int i= newVars.length-1 ; i>=0 ; i--) {
            int prev = newValues[i];
            cur = newVars[i].getNodeFree(prev, cur);
        }

        return cur;
    }


    public LogicalModel getModel() {
        return new LogicalModelImpl( newDDM, newCore, newCoreFunctions, newExtra, newExtraFunctions);
    }
}
