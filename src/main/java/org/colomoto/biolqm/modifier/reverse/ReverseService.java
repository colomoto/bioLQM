package org.colomoto.biolqm.modifier.reverse;

import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.kohsuke.MetaInfServices;

/**
 * Wrap the reverser code into the ModelModifier interface.
 *
 * @author Aurelien Naldi
 */
@MetaInfServices(ModelModifierService.class)
public class ReverseService extends BaseService implements ModelModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "reverse";
    private static final String NAME = "model reverser";
    private static final String DESCR = "(no parameters)";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public ReverseService() {
        super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model) {
        return new ReverseModifier(model);
    }

}
