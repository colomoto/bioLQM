package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.biolqm.helper.state.StatePattern;

public class ReductionModifier extends BaseModifier {

    private final LogicalModel model;

    public boolean handleFixed = false;
    public boolean purgeFixed = false;

    public boolean handleDuplicates = false;

    public boolean handleOutputs = false;

    public boolean keepExtra = true;

    public StatePattern pattern = null;
    public boolean forcePattern = false;

    public ReductionModifier(LogicalModel model) {
        this.model = model;
    }

    public ReductionModifier(LogicalModel model, String parameters) {
        this.model = model;
        setParameters(parameters);
    }

    public void setParameters(String[] options) {
        for (String o: options) {
            // In the future, we will use a leading ":" to discriminate keywords from component names
            // For now we just ignore this character
            if (o.startsWith(":")) {
                o = o.substring(1);
            }
            if ("fixed".equalsIgnoreCase(o)) {
                handleFixed = true;
            } else if ("purge".equalsIgnoreCase(o)) {
                purgeFixed = true;
            } else if ("duplicate".equalsIgnoreCase(o)) {
                handleDuplicates = true;
            } else if ("output".equalsIgnoreCase(o)) {
                handleOutputs = true;
            } else if ("no-extra".equalsIgnoreCase(o)) {
                keepExtra = false;
            }
        }
    }

    public boolean hasReduction() {
        return (handleFixed || purgeFixed || handleDuplicates || handleOutputs || pattern != null);
    }

    @Override
    public LogicalModel performTask() throws Exception {
    	if (!hasReduction()) {
    		return model;
    	}
    	
        ModelReducer reducer = new ModelReducer(model);
        if (handleOutputs) {
            reducer.removePseudoOutputs();
        }
        LogicalModel result = reducer.getModel();

        if (pattern != null) {

        }

        if (pattern != null) {
            result = new PatternReduction(result, pattern).call();
        }
        
        if (handleFixed) {
            result = FixedComponentRemover.reduceFixed(result, purgeFixed);
        }

        if (handleDuplicates) {
            result = DuplicateRemover.removeDuplicateComponents(result);
        }

        if (!keepExtra && result.getExtraComponents().size() > 0) {
            result = result.clone(false);
        }

        return result;
    }
}
