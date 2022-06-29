package org.colomoto.biolqm.modifier.most_permissive;

import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.mddlib.*;
import org.colomoto.mddlib.internal.MDDStoreImpl;
import org.colomoto.mddlib.operators.MDDBaseOperators;



import java.util.*;

/**
 *
 * @author
 */
public class MostPermissiveModifier extends BaseModifier implements IndexMapper {
    private final LogicalModel model;
    private MDDManager ddm, newDDM;
    private PathSearcher searcher;
    private List<NodeInfo> core, extra, newCore, newExtra;
    private int[] coreFunctions, extraFunctions, newCoreFunctions, newExtraFunctions;

    private MDDMapper mapper = null;
    private int curTarget = 0;
    private int nbBuffers = 0;

    public MostPermissiveModifier(LogicalModel model) {
        this.model = model;
    }

    @Override
    public LogicalModel performTask() {

        this.ddm = model.getMDDManager();
        this.core = model.getComponents();
        this.extra = model.getExtraComponents();
        this.coreFunctions = model.getLogicalFunctions();
        this.extraFunctions = model.getExtraLogicalFunctions();
        this.searcher = new PathSearcher(this.ddm);


        List<NodeInfo> extended_components = new ArrayList<>();
        for (NodeInfo ni: core) {
            extended_components.add(new NodeInfo(ni.getNodeID()+"_a"));
            extended_components.add(new NodeInfo(ni.getNodeID()+"_b"));
            extended_components.add(new NodeInfo(ni.getNodeID()+"_c"));
        }

        int[] new_rules = new int[extended_components.size()];

        // new MDD manager with the extended variables
        byte[] new_state = new byte[3*coreFunctions.length];
        this.newDDM = new MDDStoreImpl(extended_components, 2);
//        PathSearcher new_searcher = new PathSearcher(new_ddmanager, 1, Integer.MAX_VALUE);
        int[] path = searcher.getPath();




        // Rewrite all rules in the new MDD manager
        for (int i=0 ; i<coreFunctions.length ; i++) {
            // get the current rule and assign it to the searcher
            int cur_rule = coreFunctions[i];
            int new_rule = mapRule(cur_rule, path, new_state);
            int new_neg = mapRule(ddm.not(cur_rule), path, new_state);

            int true_1 = newDDM.getNodeVariable(3*i).getNode(0,1);
            int false_1 = newDDM.not(true_1);
            int true_2 = newDDM.getNodeVariable(3*i + 1).getNode(0,1);
            int false_2 = newDDM.not(true_2);
            int true_3 = newDDM.getNodeVariable(3*i + 2).getNode(0,1);
            int false_3 = newDDM.not(true_3);

            // Rules v1
            int tmp1 = MDDBaseOperators.AND.combine(newDDM, true_1, true_2);
            int tmp2 = MDDBaseOperators.AND.combine(newDDM, true_2, true_3);
            int r = MDDBaseOperators.OR.combine(newDDM, tmp1, tmp2);
            newDDM.free(tmp1);
            newDDM.free(tmp2);
            int tmp = MDDBaseOperators.AND.combine(newDDM, false_2, true_3);
            tmp1 = MDDBaseOperators.AND.combine(newDDM, tmp, false_1);
            tmp1 = MDDBaseOperators.AND.combine(newDDM, tmp1, new_neg);  // FIXME leak tmp1
            r = MDDBaseOperators.OR.combine(newDDM, r, tmp1); // FIXME leak r
            tmp2 = MDDBaseOperators.AND.combine(newDDM, tmp, true_1);
            tmp2 = MDDBaseOperators.AND.combine(newDDM, tmp2, newDDM.not(new_rule)); // FIXME leak tmp2 and not rule
            new_rules[3*i] = MDDBaseOperators.OR.combine(newDDM, r, tmp2); // FIXME leak r
            newDDM.free(tmp);
            newDDM.free(tmp1);
            newDDM.free(tmp2);
            newDDM.free(r);

            // Rules v2
            tmp = MDDBaseOperators.AND.combine(newDDM, true_1, true_2);
            tmp1= MDDBaseOperators.OR.combine(newDDM, false_3, newDDM.not(new_neg)); // FIXME leak not neg
            tmp1= MDDBaseOperators.AND.combine(newDDM, tmp, tmp1);
            tmp2 = MDDBaseOperators.AND.combine(newDDM,false_1,true_3);
            new_rules[3*i + 1] = MDDBaseOperators.OR.combine(newDDM, tmp1, tmp2);
            newDDM.free(tmp);
            newDDM.free(tmp1);
            newDDM.free(tmp2);

            // Rules v3
            




            newDDM.free(true_1);
            newDDM.free(true_2);
            newDDM.free(true_3);
            newDDM.free(false_1);
            newDDM.free(false_2);
            newDDM.free(false_3);




        }

        // FIXME

        return new LogicalModelImpl(extended_components, newDDM, new_rules);
    }

    @Override
    public int get(int i) {
        return i;
    }


    public int mapRule ( int cur_rule, int[] path, byte[] new_state) {
        searcher.setNode(cur_rule);

        // create a new rule, start with false (MDD = 0)
        int new_rule = 0;

        for (int v : searcher) {
            int new_idx = 0;
            for (int value : path) {
                if (value < 0) {
                    new_state[new_idx++] = -1;
                    new_state[new_idx++] = -1;
                    new_state[new_idx++] = -1;
                } else if (value == 0) {
                    new_state[new_idx++] = -1;
                    new_state[new_idx++] = 0;
                    new_state[new_idx++] = -1;
                } else {
                    new_state[new_idx++] = -1;
                    new_state[new_idx++] = -1;
                    new_state[new_idx++] = 1;
                }
            }
            System.out.println();

            // Build a MDD for this condition and add it to the new rule
            // do not forget to free the old MDDs
            int mdd_extended_condition = newDDM.nodeFromState(new_state, 1);
            int tmp = MDDBaseOperators.OR.combine(newDDM, new_rule, mdd_extended_condition);

            newDDM.free(mdd_extended_condition);
            newDDM.free(new_rule);
            new_rule = tmp;
        }
        return new_rule;
    }

}

