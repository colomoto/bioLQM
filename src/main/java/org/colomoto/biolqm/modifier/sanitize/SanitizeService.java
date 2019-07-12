package org.colomoto.biolqm.modifier.sanitize;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

/**
 * A service for model cleanups and sanity checks.
 *
 * It currently allows to build valid IDs based on components
 * names, improving the readability of some SBML models.
 *
 * @author Aurelien Naldi
 */
@MetaInfServices(ModelModifierService.class)
public class SanitizeService extends BaseService implements ModelModifierService {

    /** The identifier used to retrieve this service by name */
    public static final String ID = "sanitize";
    private static final String NAME = "sanity cleanups";
    private static final String DESCR = ":name2id";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public SanitizeService() {
        super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model) {
        return new SanitizeModifier(model);
    }

}
