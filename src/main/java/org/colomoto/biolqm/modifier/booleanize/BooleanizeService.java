package org.colomoto.biolqm.modifier.booleanize;

import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.kohsuke.MetaInfServices;

/**
 * A service for model booleanization.
 *
 * @author Aurelien Naldi
 */
@MetaInfServices(ModelModifierService.class)
public class BooleanizeService extends BaseService implements ModelModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "booleanize";
    private static final String NAME = "model booleanizer";
    private static final String DESCR = "(no parameters)";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public BooleanizeService() {
        super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }

    public ModelModifier getModifier(LogicalModel model) {
        return new BooleanizeModifier(model);
    }

}
