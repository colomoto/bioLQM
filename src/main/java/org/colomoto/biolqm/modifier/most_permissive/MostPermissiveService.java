package org.colomoto.biolqm.modifier.most_permissive;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.modifier.buffer.BufferingModifier;
import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(ModelModifierService.class)
public class MostPermissiveService extends BaseService implements ModelModifierService {
    /** The identifier used to retrieve this service by name */
    public static final String ID = "most_permissive";

    public static final String[] ALIASES = { "mp" };
    private static final String NAME = "extension reproducing the most permissive semantics";
    private static final String DESCR = "(no parameters)";

    /**
     * Public constructor which should only be used for service discovery.
     */
    public MostPermissiveService() {
        super(ID, ALIASES, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
    }


    @Override
    public ModelModifier getModifier(LogicalModel model) {
        return new MostPermissiveModifier(model);
   }
}
