package org.colomoto.biolqm.modifier.subspace;

import org.colomoto.biolqm.service.LQMServiceManager;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.perturbation.PerturbationService;
import org.colomoto.biolqm.modifier.reduction.ReductionService;
import org.colomoto.biolqm.modifier.reduction.ReductionModifier;

/**
 * Model Modifier to restrict a model into a subspace.
 * It is a shortcut to fix a subset of the components, propagate fixed values and purge them from the modified model
 * 
 * @author Aurelien Naldi
 */
public class SubSpaceModifier implements ModelModifier {

    private static final ReductionService reduceService = LQMServiceManager.get(ReductionService.class);
    private static final PerturbationService perturbationService = LQMServiceManager.get(PerturbationService.class);

    private final LogicalModel model;
	private byte[] pattern;

	public SubSpaceModifier(LogicalModel model, byte[] pattern) {
		this.model = model;
		setPattern(pattern);
	}

	public SubSpaceModifier(LogicalModel model) {
		this.model = model;
	}

	public void setParameters(String[] params) {
		int n = model.getComponents().size();
		byte[] pattern = new byte[n];
		for (int i=0 ; i<n ; i++) {
			pattern[i] = -1;
		}

		if (params.length == 1) {
			String parameters = params[0];

			if (n == parameters.length()) {
				System.out.println("RESTRICT to ["+parameters+"]");
				for (int i=0 ; i<n ; i++) {
					char cur = parameters.charAt(i);
					if (cur == '0') {
						pattern[i] = 0;
					} else if (cur == '1') {
						pattern[i] = 1;
					}
				}
				setPattern(pattern);
				return;
			}

			// TODO: parse restriction with named components
			System.out.println("no restriction applied");
		}
	}

	public void setPattern(byte[] pattern) {
		this.pattern = pattern;
	}
	
	@Override
	public LogicalModel getModifiedModel() {
		LogicalModel modified = model.clone();
		int[] functions = modified.getLogicalFunctions();
		for (int i=0 ; i<pattern.length ; i++) {
			int v = pattern[i];
			if (v >= 0) {
				functions[i] = v;
			}
		}

		ReductionModifier modifier = reduceService.getModifier(modified);
		modifier.handleFixed = true;
		modifier.purgeFixed = true;
		return modifier.getModifiedModel();
	}

}
