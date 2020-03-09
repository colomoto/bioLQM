package org.colomoto.biolqm.modifier.buffer;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

/**
 * A service to add buffer components.
 *
 * @author Aurelien Naldi
 */
@MetaInfServices(ModelModifierService.class)
public class BufferingService extends BaseService implements ModelModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "buffer";
    private static final String NAME = "add buffer components to a model";
    private static final String DESCR = ":buffer | :delay | source:target1,target2";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public BufferingService() {
        super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }

    public ModelModifier getModifier(LogicalModel model) {
        return new BufferingModifier(model);
    }

}
