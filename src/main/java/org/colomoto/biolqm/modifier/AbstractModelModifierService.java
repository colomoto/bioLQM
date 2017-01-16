package org.colomoto.biolqm.modifier;


import org.colomoto.biolqm.LogicalModel;

/**
 * A base class handling service discovery for model modifier services.
 *
 * @author Aurelien Naldi
 */
abstract public class AbstractModelModifierService implements ModelModifierService {

    private final String id, name, descr;

    public AbstractModelModifierService(String id, String name, String descr) {
        this.id = id;
        this.name = name;
        this.descr = descr;
    }

    @Override
    public String getID() {
        return id;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getDescription() {
        return descr;
    }

    @Override
    public LogicalModel getModifiedModel(LogicalModel model, String parameters) {
        return getModifier(model,parameters).getModifiedModel();
    }
}
