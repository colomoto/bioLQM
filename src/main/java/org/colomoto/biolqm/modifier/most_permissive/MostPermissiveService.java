package org.colomoto.biolqm.modifier.most_permissive;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.modifier.buffer.BufferingModifier;
import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(ModelModifierService.class)

public abstract class MostPermissiveService extends BaseService implements ModelModifierService {
    /** The identifier used to retrieve this service by name */
    public static final String ID = "buffer";
    private static final String NAME = "add buffer components to a model";
    private static final String DESCR = ":buffer | :delay | source:target1,target2";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public MostPermissiveService() {
        super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }


}
