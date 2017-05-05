package org.colomoto.biolqm.modifier.subspace;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.AbstractModelModifierService;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Modifier Service for subspace restriction
 * 
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class SubSpaceService extends AbstractModelModifierService {

	/** The identifier used to retrieve this service by name */
    public static final String ID = "restrict";
    private static final String NAME = "Restrict a model into a subspace";
    private static final String DESCR = "pattern, i.e. 00----1--0-1-";

    public SubSpaceService() {
		super(ID, NAME, DESCR);
	}

	@Override
	public ModelModifier getModifier(LogicalModel model, String parameters) {
		System.out.println("RESTRICT to ["+parameters+"]");
		int n = model.getComponents().size();
		byte[] pattern = new byte[n];
		for (int i=0 ; i<n ; i++) {
			pattern[i] = -1;
		}
		
		if (n == parameters.length()) {
			for (int i=0 ; i<n ; i++) {
				char cur = parameters.charAt(i);
				if (cur == '0') {
					pattern[i] = 0;
				} else if (cur == '1') {
					pattern[i] = 1;
				}
			}
		} else {
			System.out.println("no restriction applied");
		}
		return new SubSpaceRestriction(model, pattern);
	}
}
