package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.biolqm.helper.state.Range;
import org.colomoto.biolqm.helper.state.StatePattern;
import org.colomoto.mddlib.MDDManager;

public class PatternReduction extends BaseModifier {

    private final LogicalModel model;
    private final StatePattern pattern;

    public PatternReduction(LogicalModel model, StatePattern pattern) {
        this.model = model;
        this.pattern = pattern;
    }

    @Override
    public LogicalModel performTask() {

        LogicalModel modified = model.clone();
        MDDManager ddmanager = modified.getMDDManager();
        int[] functions = modified.getLogicalFunctions();
        int i = -1;
        for (NodeInfo ni: modified.getComponents()) {
            i++;
            Range range = pattern.get(ni);
            if (range == null || range.min != range.max) {
                continue;
            }

            int v = range.min;
            if (v < 0 || v != range.max || v > ni.getMax()) {
                continue;
            }

            // TODO: add flag to only restrict sustained input nodes

            // Do not override a component which is already fixed
            int f = functions[i];
            if (!ddmanager.isleaf(f)) {
                functions[i] = v;
            }
        }

        return modified;
    }

}
