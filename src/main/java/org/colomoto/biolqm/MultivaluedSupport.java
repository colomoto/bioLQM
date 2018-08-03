package org.colomoto.biolqm;

public 	enum MultivaluedSupport {

    BOOLEAN_STRICT("b"), BOOLEANIZED("B"), MULTIVALUED("M");

    public final String flag;

    MultivaluedSupport(String flag) {
        this.flag = flag;
    }

};
