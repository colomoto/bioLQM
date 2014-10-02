package org.colomoto.logicalmodel.tool.booleanize;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
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
        this.newDDM = new MDDStoreImpl( newCore, ddm.getLeafCount() );
        this.newCoreFunctions = new int[ newCore.size() ];
        this.newExtraFunctions = new int[ newExtra.size() ];

        transformFunctions(core, coreFunctions, newCoreFunctions);
        transformFunctions(extra, extraFunctions, newExtraFunctions);
    }

    public static List<NodeInfo> addComponents( List<NodeInfo> nodes, Map<NodeInfo, List<NodeInfo>> mv2bool) {

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
                targetFunctions[t++] = transform(f);
            } else {
                for (int i=0 ; i<bnodes.size() ; i++) {
                    targetFunctions[t++] = transform(f, i);
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
    public int transform(int f) {

        // TODO: transform a function

        return 0;
    }

    /**
     * Take a Multivalued function and transfer it into the new MDDManager,
     * replacing the multi-valued regulators if any.
     *
     * @param f MDD in the original MDDManager
     * @param v the currently selected value
     * @return a new MDD in the Boolean MDDManager
     */
    public int transform(int f, int v) {

        // TODO: transform a function

        return 0;
    }


    public LogicalModel getModel() {
        return new LogicalModelImpl( newDDM, newCore, newCoreFunctions, newExtra, newExtraFunctions);
    }
}
