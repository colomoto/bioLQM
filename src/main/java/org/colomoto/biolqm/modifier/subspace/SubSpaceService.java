package org.colomoto.biolqm.modifier.subspace;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModifierService;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Modifier Service for subspace restriction
 * 
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class SubSpaceService extends AbstractModifierService {

	/** The identifier used to retrieve this service by name */
    public static final String ID = "restrict";
    private static final String NAME = "Restrict a model into a subspace";
    private static final String DESCR = "pattern, i.e. 00----1--0-1-";

    public SubSpaceService() {
		super(ID, NAME, DESCR);
	}

	@Override
	public ModelModifier getModifier(LogicalModel model, String parameters) {
		return new SubSpaceModifier(model, parameters);
	}
}
