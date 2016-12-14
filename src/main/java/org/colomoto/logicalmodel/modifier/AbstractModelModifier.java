package org.colomoto.logicalmodel.modifier;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Created by aurelien on 12/14/16.
 */
abstract public class AbstractModelModifier implements ModelModifierService {

    private final String id, name;

    public AbstractModelModifier(String id, String name) {
        this.id = id;
        this.name = name;
    }

    @Override
    public String getID() {
        return id;
    }

    @Override
    public String getName() {
        return name;
    }
}
