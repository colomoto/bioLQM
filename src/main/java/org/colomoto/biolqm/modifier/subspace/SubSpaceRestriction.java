package org.colomoto.biolqm.modifier.subspace;

import org.colomoto.biolqm.LQMServiceManager;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.perturbation.PerturbationService;
import org.colomoto.biolqm.modifier.reduction.ModelReductionService;
import org.colomoto.biolqm.modifier.reduction.ReductionSettings;

/**
 * Model Modifier to restrict a model into a subspace.
 * It is a shortcut to fix a subset of the components, propagate fixed values and purge them from the modified model
 * 
 * @author Aurelien Naldi
 */
public class SubSpaceRestriction implements ModelModifier {

    private static final ModelReductionService reduceService = LQMServiceManager.getModifier(ModelReductionService.class);
    private static final PerturbationService perturbationService = LQMServiceManager.getModifier(PerturbationService.class);

    private final LogicalModel model;
	private final byte[] pattern;

	public SubSpaceRestriction(LogicalModel model, byte[] pattern) {
		this.model = model;
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
		ReductionSettings settings = reduceService.getSettings();
		settings.handleFixed = true;
		settings.purgeFixed = true;
		return reduceService.getModifier(modified, settings).getModifiedModel();
	}

}
