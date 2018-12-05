package org.colomoto.biolqm.helper.state;

import org.colomoto.biolqm.NodeInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Group a list of states after projecting it on a subset of components.
 *
 * @author Aurelien Naldi
 */
public class StateListProjection extends ArrayList<byte[]> implements StateList {

    StateList parent;
    int[] selectedComponents;
    NodeInfo[] components = null;

    public StateListProjection(StateList parent, Iterable<String> selected) {
        this(parent, get_selection(parent.getComponents(), selected));
    }

    public StateListProjection(StateList parent, int[] selectedComponents) {
        this.parent = parent;
        this.selectedComponents = selectedComponents;
        this.refresh();
    }

    private void refresh() {
        // extract the list of components from the parent
        this.components = new NodeInfo[selectedComponents.length];
        NodeInfo[] parentComponents = this.parent.getComponents();
        for (int i=0 ; i< this.components.length ; i++) {
            this.components[i] = parentComponents[ this.selectedComponents[i] ];
        }

        // reset the internal list of projected states
        this.clear();

        // project all parent states and collect unique projected states
        int n = parent.size();
        byte[] parentState = new byte[parent.getComponents().length];
        Map<String, Integer> projectionMap = new HashMap<>();
        byte[] projectedState = new byte[components.length];
        // We construct String signatures of the projected states for indexing in a hashmap.
        StringBuilder sb = new StringBuilder(selectedComponents.length);
        for (int i=0 ; i<n ; i++) {
            sb.setLength(0);
            parent.fillState(parentState, i);
            int k = 0;
            for (int idx: this.selectedComponents) {
                byte b = parentState[idx];
                sb.append(b);
                projectedState[k++] = b;
            }

            // retrieve or create a unique uid for the projected state
            String signature = sb.toString();
            Integer uid = projectionMap.get(signature);
            if (uid == null) {
                uid = this.size();
                byte[] stored = projectedState.clone();
                this.add(stored);
                projectionMap.put(signature,uid);
            }

        }
    }

    @Override
    public NodeInfo[] getComponents() {
        return components;
    }

    @Override
    public byte get(int row, int col) {
        return get(row)[col];
    }

    @Override
    public boolean setExtra(boolean extra) {
        return this.parent.setExtra(extra);
    }

    @Override
    public byte[] fillState(byte[] state, int index) {
        byte[] innerstate = get(index);
        if (state == null || state.length != innerstate.length) {
            state = new byte[innerstate.length];
        }
        System.arraycopy(innerstate, 0, state, 0, innerstate.length);
        return state;
    }

    public static int[] get_selection(NodeInfo[] components, Iterable<String> sel) {
        boolean[] selected = new boolean[components.length];
        int n = 0;
        for (String uid: sel) {
            int idx = 0;
            for (NodeInfo ni: components) {
                if (ni.getNodeID().equals(uid)) {
                    if (!selected[idx]) {
                        selected[idx] = true;
                        n++;
                    }
                    break;
                }
                idx++;
            }
        }

        int[] projection = new int[n];
        int k = 0;
        for (int idx=0 ; idx<selected.length ; idx++) {
            if (selected[idx]) {
                projection[k++] = idx;
            }
        }

        return projection;
    }
}
