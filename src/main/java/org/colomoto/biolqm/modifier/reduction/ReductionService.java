package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.kohsuke.MetaInfServices;

/**
 * Orchestrator for model reduction tools
 *
 * @author Aurelien Naldi
 */
@MetaInfServices(ModelModifierService.class)
public class ReductionService extends BaseService implements ModelModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "reduce";
    private static final String NAME = "model reduction";
    private static final String DESCR = ":fixed | :duplicate | :output";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public ReductionService() {
        super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }

    @Override
    public ReductionModifier getModifier(LogicalModel model) {
        return new ReductionModifier(model);
    }

}
