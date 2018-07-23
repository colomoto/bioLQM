package org.colomoto.biolqm.modifier;


import org.colomoto.biolqm.BaseService;
import org.colomoto.biolqm.LogicalModel;

/**
 * A base class handling service discovery for model modifier services.
 *
 * @author Aurelien Naldi
 */
abstract public class AbstractModifierService extends BaseService implements ModelModifierService {

    /**
     * Shared constructor for model modifier.
     * 
     * @param id the identifier used to retrieve the service instance
     * @param name the human readable name of the service
     * @param descr a longer description (for the help message)
     */
    public AbstractModifierService(String id, String name, String descr) {
        super(id, name, descr);
    }

    public AbstractModifierService(String id, String[] aliases, String name, String descr) {
        super(id, aliases, name, descr);
    }

    @Override
    public LogicalModel getModifiedModel(LogicalModel model, String parameters) {
        return getModifier(model,parameters).getModifiedModel();
    }
}
