package org.colomoto.biolqm.helper.state;

import org.colomoto.biolqm.NodeInfo;

public interface StateList {

    int JOKER = -1;
    int FREE = -2;
    int UNDEFINED = -5;

    /**
     * Get the components defining this list of states.
     * @return the list of components
     */
    NodeInfo[] getComponents();

    /**
     * get the number of states or patterns in the list.
     *
     * @return the number of states/patterns
     */
    int size();

    /**
     * Retrieve a specific value
     *
     * @param row the state number
     * @param col the component number
     * @return the corresponding value
     */
    byte get(int row, int col);

    /**
     * Include or hide extra components.
     *
     * @param extra if true, extra components will be included
     * @return true if the number of components changed
     */
    boolean setExtra(boolean extra);

    /**
     * Retrieve a full state instead of a single value.
     *
     * @param state reuse an existing array if possible
     * @param index the state number
     * @return a filled array, if possible it will be identical to the one given as parameter
     */
    byte[] fillState(byte[] state, int index);


    /**
     * Project the list of states on a subset of components
     *
     * @param selectedComponents
     * @return
     */
    default StateListProjection project_on_subset(Iterable<String> selectedComponents) {
        return new StateListProjection(this, selectedComponents);
    }


}
