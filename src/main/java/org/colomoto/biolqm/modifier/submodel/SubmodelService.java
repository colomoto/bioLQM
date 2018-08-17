package org.colomoto.biolqm.modifier.submodel;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(ModelModifierService.class)
public class SubmodelService extends BaseService implements ModelModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "submodel";
    private static final String NAME = "extract sub-model";
    private static final String DESCR = "list of component IDs";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public SubmodelService() {
        super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }

    public ModelModifier getModifier(LogicalModel model) {
        return new SubmodelModifier(model);
    }

}
