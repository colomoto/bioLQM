package org.colomoto.biolqm.tool.simulation.ordering;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.BlockSequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;

import java.util.List;

/**
 * Apply a simple ordering on groups of components for deterministic updaters.
 * This ordered list of groups can yield sequential or priority updaters.
 */
public class DeterministicGrouping extends Grouping {

    public DeterministicGrouping(LogicalModel model) {
        super(model);
    }

    public DeterministicGrouping(LogicalModel model, String setup) {
        super(model);
        boolean first = true;
        for (String block: setup.split(";")) {
            if (first) {
                first = false;
                continue;
            }
            Group group = new Group(this);
            add(group);
            for (String component: block.split(",")) {
                String uid = component.trim();
                SplittingType type = SplittingType.MERGED;
                if (uid.endsWith("[+]")) {
                    uid = uid.substring(0, uid.length()-3);
                    type = SplittingType.POSITIVE;
                } else if (uid.endsWith("[-]")) {
                    uid = uid.substring(0, uid.length()-3);
                    type = SplittingType.NEGATIVE;
                }
                NodeInfo ni = model.getComponent(uid);
                if (ni == null) {
                    continue;
                }
                assign(ni, type, group);
            }
        }

    }

    public DeterministicUpdater getBlockSequentialUpdater() {

        List<NodeInfo> nodes = model.getComponents();
        int[] blocks = new int[nodes.size()];
        int idx = 0;
        for (NodeInfo ni: nodes) {
            NodeMembership membership = memberships.get(ni);
            int j = indexOf( membership.getGroup(SplittingType.MERGED));
            if (j < 0) {
                blocks[idx++] = 0;
            } else {
                blocks[idx++] = j;
            }
        }
        return new BlockSequentialUpdater(model, blocks);
    }


}
