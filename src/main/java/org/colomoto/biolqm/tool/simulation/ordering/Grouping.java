package org.colomoto.biolqm.tool.simulation.ordering;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

import java.util.*;

public class Grouping {

    private final LogicalModel model;
    private Map<NodeInfo, NodeMembership> memberships = new HashMap();
    private final List<Group> groups;

    public Grouping(LogicalModel model) {
        this.model = model;
        this.groups = new ArrayList<Group>();
        refresh();
    }

    public void assign(NodeInfo ni, SplittingType type, Group grp) {
        NodeMembership membership = memberships.get(ni);
        if (grp == null) {
            if (membership == null) {
                return;
            }
            membership.assign(type, grp);
            return;
        }

        // Adding a new component
        if (membership == null) {
            membership = new NodeMembership(ni, grp);
            memberships.put(ni, membership);
            return;
        }

        membership.assign(type, grp);
    }

    public void refresh() {
        // Cleanup old nodes
        Set<NodeInfo> modelNodes = new HashSet<NodeInfo>(model.getComponents());
        for (NodeInfo ni : memberships.keySet()) {
            if (!modelNodes.contains(ni)) {
                assign(ni, SplittingType.MERGED, null);
            }
        }

        // Make sure to have all nodes
        if (groups.size() < 1) {
            groups.add(new Group(this));
        }
        Group grp = groups.get(0);
        for (NodeInfo ni : model.getComponents()) {
            if (!memberships.containsKey(ni)) {
                assign(ni, SplittingType.MERGED, grp);
            }
        }
    }

}
