package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;

public class InitialStateFactory {

    public static byte[] parseInitialState(LogicalModel model, String s) {
        int n = s.length();
        int k = model.getComponents().size();
        if (n != k) {
            throw new RuntimeException("Length of initial state mismatch: "+n + " (expected: "+k+")");
        }

        byte[] state = new byte[n];
        for (int i=0 ; i<n ; i++) {
            state[i] = (byte)Character.getNumericValue(s.charAt(i));
        }

        return state;
    }


}
