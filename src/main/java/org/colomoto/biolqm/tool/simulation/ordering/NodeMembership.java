package org.colomoto.biolqm.tool.simulation.ordering;

import org.colomoto.biolqm.NodeInfo;

public class NodeMembership {

    public final NodeInfo node;
    private GroupMember posGroup, negGroup;

    protected NodeMembership(NodeInfo node, Group group) {
        this.node = node;
        assign(SplittingType.MERGED, group);
    }

    public void assign(SplittingType type, Group grp) {
        // FIXME: remove from group
        // FIXME: update group
    }
}
