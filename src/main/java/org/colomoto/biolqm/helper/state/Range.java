package org.colomoto.biolqm.helper.state;

public class Range {

    public int min;
    public int max;

    public Range() {
        this(0, -1);
    }

    public Range(int min, int max) {
        this.min = min;
        this.max = max;
    }

    public Range clone() {
        return new Range(min, max);
    }
}
