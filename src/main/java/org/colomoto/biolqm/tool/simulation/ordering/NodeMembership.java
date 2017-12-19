package org.colomoto.biolqm.tool.simulation.ordering;

import org.colomoto.biolqm.NodeInfo;

public class NodeMembership {

    public final NodeInfo node;
    private GroupMember posGroup, negGroup;

    protected NodeMembership(NodeInfo node, Group group) {
        this.node = node;
        posGroup = new GroupMember(this, SplittingType.MERGED);
        setGroup(SplittingType.MERGED, group);
    }

    public Group getGroup(SplittingType type) {
        if (negGroup == null || type != SplittingType.NEGATIVE) {
            return posGroup.group;
        }
        return negGroup.group;
    }

    public void setGroup(SplittingType type, Group grp) {
        if (type == SplittingType.MERGED) {
            if (negGroup != null) {
                negGroup.setGroup(null);
                negGroup = null;
            }
            posGroup.type = type;
            posGroup.setGroup(grp);
            return;
        }

        if (negGroup == null) {
            posGroup.type = SplittingType.POSITIVE;
            negGroup = new GroupMember(this, SplittingType.NEGATIVE);
            if (type == SplittingType.NEGATIVE) {
                negGroup.setGroup(grp);
            } else {
                negGroup.setGroup(posGroup.group);
            }
            return;
        }

        if (type == SplittingType.NEGATIVE) {
            negGroup.setGroup(grp);
        } else {
            posGroup.setGroup(grp);
        }

    }
}
