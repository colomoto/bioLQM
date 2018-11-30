package org.colomoto.biolqm.helper.state;

import java.util.Iterator;

/**
 * Iterator over all states covered by a state pattern.
 * This iterator expands a pattern into individual states, it supports multi-valued components.
 * All generated states use the same memory slot, users of this iterator should clone them as needed.
 *
 * @author Aurelien Naldi
 */
public class PatternStateIterator implements Iterator<byte[]> {

    private final byte[] maxs;

    private byte[] next = null;
    private int[] jokers = null;
    private int nextJoker;

    /**
     * Create an iterator over all states in a Boolean pattern.
     *
     * @param pattern the pattern to expand. jokers are represented by negative values.
     */
    public PatternStateIterator(byte[] pattern) {
        this.maxs = new byte[pattern.length];
        for (int i=0 ; i<maxs.length ; i++) {
            maxs[i] = 1;
        }
        reset(pattern);
    }

    /**
     * Create an iterator over all states in a multi-valued pattern.
     *
     * @param pattern the pattern to expand. Jokers are represented by negative values.
     * @param maxs the max value of all components in the pattern
     */
    public PatternStateIterator(byte[] pattern, byte[] maxs) {
        this.maxs = maxs;
        reset(pattern);
    }

    /**
     * Assign a new pattern to this iterator and start over.
     *
     * @param pattern the new pattern to expand. Jokers are represented by negative values.
     */
    public void reset(byte[] pattern) {
        // Prepare the iterator: collect and reset joker positions
        this.next = pattern.clone();
        int nb_jokers = 0;
        for (byte v: pattern) {
            if (v == StateList.JOKER) {
                nb_jokers++;
            }
        }

        jokers = new int[nb_jokers];
        int idx_joker = 0;
        for (int idx=0 ; idx_joker<nb_jokers ; idx++) {
            if (pattern[idx] == StateList.JOKER) {
                jokers[idx_joker++] = idx;
                next[idx] = 0;
            }
        }
        nextJoker = 0;
    }

    /**
     * Assign a new pattern to this iterator and start over.
     *
     * @param pattern the new pattern to expand. Jokers are represented by negative values.
     */
    public void reset(int[] pattern) {
        byte[] bpattern = new byte[pattern.length];
        for (int i=0 ; i<pattern.length ; i++) {
            bpattern[i] = (byte)pattern[i];
        }
        this.reset(bpattern);
    }

    private void buildNext() {
        // Find the first joker position which can be increased
        for (int j: jokers) {
            if (next[j] < maxs[j]) {
                next[j]++;
                return;
            }
            // reset all previous positions
            next[j] = 0;
        }

        // All jokers are exhausted: stop here
        next = null;
    }

    @Override
    public boolean hasNext() {
        return next != null;
    }

    @Override
    public byte[] next() {
        byte[] ret = next.clone();
        buildNext();
        return ret;
    }

}
