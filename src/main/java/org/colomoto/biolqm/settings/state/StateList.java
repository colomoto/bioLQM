package org.colomoto.biolqm.settings.state;

import org.colomoto.biolqm.NodeInfo;

public interface StateList {

    NodeInfo[] getComponents();

    int size();

    int get(int row, int col);

    void setExtra(boolean extra);

    byte[] fillState(byte[] state, int index);
}
