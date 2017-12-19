package org.colomoto.biolqm.tool.simulation.ordering;

public class GroupMember {
    public final NodeMembership membership;
    public SplittingType type;
    public Group group;

    public GroupMember(NodeMembership membership, SplittingType type) {
        this.membership = membership;
        this.type = type;
    }

    public String toString() {
        return membership.node.getNodeID() + type;
    }

    public void setGroup(Group group) {
        if (group == this.group) {
            return;
        }

        if (this.group != null) {
            this.group.remove(this);
        }

        this.group = group;

        if (this.group != null) {
            this.group.add(this);
        }

}

    public boolean isSplit() {
        return (type != SplittingType.MERGED);
    }

    public void split() {
        if (isSplit()) {
            return;
        }

        this.type = SplittingType.POSITIVE;
        membership.setGroup(SplittingType.NEGATIVE, group);
    }

    public void merge() {
        if (type == SplittingType.MERGED) {
            return;
        }

        membership.setGroup(SplittingType.MERGED, group);
    }

}
