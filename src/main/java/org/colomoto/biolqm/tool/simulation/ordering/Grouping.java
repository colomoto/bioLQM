package org.colomoto.biolqm.tool.simulation.ordering;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

import java.util.*;

/**
 * Define a grouping of transitions on components, optionally separating
 * increases from decreases.
 * This grouping can serve as basis for some configurable updatings, in particular
 * using priorities or sequential updatings.
 *
 * The data structure defining the grouping goes beyond an ordered list of component
 * identifiers to facilitate its integration in model editors and GUIs.
 *
 * @author Aurelien Naldi
 */
public class Grouping extends ArrayList<Group> {

    protected final LogicalModel model;
    protected Map<NodeInfo, NodeMembership> memberships = new HashMap();

    public Grouping(LogicalModel model) {
        this.model = model;
        refresh();
    }

    public void assign(NodeInfo ni, SplittingType type, Group grp) {
        NodeMembership membership = memberships.get(ni);
        if (grp == null) {
            if (membership == null) {
                return;
            }
            membership.setGroup(type, grp);
            return;
        }

        // Adding a new component
        if (membership == null) {
            membership = new NodeMembership(ni, grp);
            memberships.put(ni, membership);
            return;
        }

        membership.setGroup(type, grp);
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
        if (size() < 1) {
            add(new Group(this));
        }
        Group grp = get(0);
        for (NodeInfo ni : model.getComponents()) {
            if (!memberships.containsKey(ni)) {
                assign(ni, SplittingType.MERGED, grp);
            }
        }

        // no need to update the content of groups as long as we used the API ?

        // remove empty groups
        List<Group> toRemove = null;
        for (Group group: this) {
            if (group.size() < 1) {
                if (toRemove == null) {
                    toRemove = new ArrayList<Group>();
                }
                toRemove.add(group);
            }
        }
        if (toRemove != null) {
            removeAll(toRemove);
        }
    }

}
