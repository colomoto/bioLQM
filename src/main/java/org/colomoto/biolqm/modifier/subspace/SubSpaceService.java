package org.colomoto.biolqm.modifier.subspace;

import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.kohsuke.MetaInfServices;

/**
 * Modifier Service for subspace restriction
 * 
 * @author Aurelien Naldi
 */
@MetaInfServices(ModelModifierService.class)
public class SubSpaceService extends BaseService implements ModelModifierService {

	/** The identifier used to retrieve this service by name */
    public static final String ID = "restrict";
    private static final String NAME = "Restrict a model into a subspace";
    private static final String DESCR = "pattern, i.e. 00----1--0-1-";

    public SubSpaceService() {
		super(ID, NAME, DESCR, MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public ModelModifier getModifier(LogicalModel model) {
		return new SubSpaceModifier(model);
	}
}
