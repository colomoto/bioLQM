package org.colomoto.biolqm.modifier.most_permissive;

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
 * @author Laure de Chancel
 * @author Aurelien Naldi
 */
public class MostPermissiveModifier extends BaseModifier {
    private final LogicalModel model;
    private MDDManager ddm, newDDM;
    private PathSearcher searcher;
    private List<NodeInfo> core, extra, newCore, newExtra;
    private int[] coreFunctions, extraFunctions, newCoreFunctions, newExtraFunctions;

    private MappedIndex[] mapping;


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

        // TODO: select the subset of extended components
        boolean[] extended = new boolean[coreFunctions.length];
        Arrays.fill(extended, true);
        
        this.mapping = new MappedIndex[coreFunctions.length];
        List<NodeInfo> extended_components = new ArrayList<>();
        int index = 0;
        for (NodeInfo ni: core) {
            boolean is_extended = extended[index];
            mapping[index] = new MappedIndex(extended_components.size(), is_extended);
            if (is_extended) {
                extended_components.add(new NodeInfo(ni.getNodeID()+"_a"));
                extended_components.add(new NodeInfo(ni.getNodeID()+"_b"));
                extended_components.add(new NodeInfo(ni.getNodeID()+"_c"));
            } else {
                extended_components.add(new NodeInfo(ni.getNodeID()));
            }
            index++;
        }

        int[] new_rules = new int[extended_components.size()];

        // new MDD manager with the extended variables
        byte[] new_state = new byte[new_rules.length];
        this.newDDM = new MDDStoreImpl(extended_components, 2);
        int[] path = searcher.getPath();

        // Rewrite all rules in the new MDD manager
        for (int idx=0 ; idx<coreFunctions.length ; idx++) {
            int cur_rule = coreFunctions[idx];
            int new_rule = mapRule(cur_rule, path, new_state);

            // Check if the component is extended and its mapping position
            MappedIndex mi = this.mapping[idx];
            if (!mi.extended) {
                new_rules[mi.index] = new_rule;
                continue;
            }

            // get the current rule and assign it to the searcher
            int tmp = ddm.not(cur_rule);
            int new_neg = mapRule(tmp, path, new_state);
            int not_new_rule = newDDM.not(new_rule);
            int not_new_neg = newDDM.not(new_neg);
            ddm.free(tmp);

            int true_1 = newDDM.getVariableForKey(extended_components.get(mi.index)).getNode(0,1);
            int false_1 = newDDM.not(true_1);
            int true_2 = newDDM.getVariableForKey(extended_components.get(mi.index + 1)).getNode(0,1);
            int false_2 = newDDM.not(true_2);
            int true_3 = newDDM.getVariableForKey(extended_components.get(mi.index + 2)).getNode(0,1);
            int false_3 = newDDM.not(true_3);

            // Rules v1
            int tmp1 = MDDBaseOperators.AND.combine(newDDM, true_1, true_2);
            int tmp2 = MDDBaseOperators.AND.combine(newDDM, true_2, true_3);
            int r = MDDBaseOperators.OR.combine(newDDM, tmp1, tmp2);
            newDDM.free(tmp1);
            newDDM.free(tmp2);
            tmp = MDDBaseOperators.AND.combine(newDDM, false_2, true_3);
            tmp2 = MDDBaseOperators.AND.combine(newDDM, tmp, false_1);
            tmp1 = MDDBaseOperators.AND.combine(newDDM, tmp2, new_neg);
            newDDM.free(tmp2);
            tmp2 = r;
            r = MDDBaseOperators.OR.combine(newDDM, tmp2, tmp1);
            newDDM.free(tmp1);
            newDDM.free(tmp2);
            tmp1 = MDDBaseOperators.AND.combine(newDDM, tmp, true_1);
            tmp2 = MDDBaseOperators.AND.combine(newDDM, tmp1, not_new_rule);
            newDDM.free(tmp1);
            new_rules[mi.index] = MDDBaseOperators.OR.combine(newDDM, r, tmp2);
            newDDM.free(tmp);
            newDDM.free(tmp2);
            newDDM.free(r);

            // Rules v2
            tmp = MDDBaseOperators.AND.combine(newDDM, true_1, true_2);
            tmp2= MDDBaseOperators.OR.combine(newDDM, false_3, not_new_neg);
            tmp1= MDDBaseOperators.AND.combine(newDDM, tmp, tmp2);
            newDDM.free(tmp);
            newDDM.free(tmp2);
            tmp2 = MDDBaseOperators.AND.combine(newDDM,false_1,true_3);
            new_rules[mi.index + 1] = MDDBaseOperators.OR.combine(newDDM, tmp1, tmp2);
            newDDM.free(tmp1);
            newDDM.free(tmp2);

            // Rules v3
            tmp = MDDBaseOperators.AND.combine(newDDM, true_1, true_2);
            tmp2 = MDDBaseOperators.AND.combine(newDDM,false_1,true_3);
            tmp1 = MDDBaseOperators.OR.combine(newDDM, tmp, tmp2);
            newDDM.free(tmp);
            newDDM.free(tmp2);
            tmp2 = MDDBaseOperators.AND.combine(newDDM, false_1, false_2);
            r = MDDBaseOperators.AND.combine(newDDM, false_3, new_rule);
            tmp = MDDBaseOperators.AND.combine(newDDM, r, tmp2);
            new_rules[mi.index + 2] = MDDBaseOperators.OR.combine(newDDM, tmp1, tmp);
            newDDM.free(r);
            newDDM.free(tmp);
            newDDM.free(tmp1);
            newDDM.free(tmp2);

            newDDM.free(true_1);
            newDDM.free(true_2);
            newDDM.free(true_3);
            newDDM.free(false_1);
            newDDM.free(false_2);
            newDDM.free(false_3);

            newDDM.free(new_rule);
            newDDM.free(new_neg);
            newDDM.free(not_new_rule);
            newDDM.free(not_new_neg);
        }

        return new LogicalModelImpl(extended_components, newDDM, new_rules);
    }


    public int mapRule ( int cur_rule, int[] path, byte[] new_state) {
        searcher.setNode(cur_rule);

        // create a new rule, start with false (MDD = 0)
        int new_rule = 0;

        for (int v : searcher) {
            if (v < 1) {
                continue;
            }
            int idx = 0;
            for (int value : path) {
                MappedIndex mi = mapping[idx];
                idx++;
                if (!mi.extended) {
                    new_state[mi.index] = (byte)value;
                    continue;
                }
                new_state[mi.index] = -1;
                if (value < 0) {
                    new_state[mi.index + 1] = -1;
                    new_state[mi.index + 2] = -1;
                } else if (value == 0) {
                    new_state[mi.index + 1] = 0;
                    new_state[mi.index + 2] = -1;
                } else {
                    new_state[mi.index + 1] = -1;
                    new_state[mi.index + 2] = 1;
                }
            }

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

class MappedIndex {
    int index;
    boolean extended;

    public MappedIndex(int index, boolean extended) {
        this.index = index;
        this.extended = extended;
    }
}
