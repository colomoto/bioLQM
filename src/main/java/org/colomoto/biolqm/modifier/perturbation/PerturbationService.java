package org.colomoto.biolqm.modifier.perturbation;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.AbstractModelModifierService;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.mangosdk.spi.ProviderFor;

/**
 * Service to apply perturbations to Logical Models.
 * This service will wrap the perturbation objects into the ModelModifierService API
 *
 * @author Aurelien Naldi
 */
@ProviderFor(ModelModifierService.class)
public class PerturbationService extends AbstractModelModifierService {

    public static final String ID = "perturbation";
    public static final String NAME = "model perturbation";
    public static final String DESCR = "Example: Node1%0,Node2%1";

    public PerturbationService() {
        super(ID, NAME, DESCR);
    }

    @Override
    public ModelModifier getModifier(LogicalModel model, String parameters) {
        return new PerturbationModifier(model, getPerturbationFromString(model, parameters));
    }

    /**
     * Construct a perturbation from the command line parameters
     * 
     * @param model
     * @param parameters
     * @return
     */
    private static LogicalModelPerturbation getPerturbationFromString(LogicalModel model, String parameters) {
    	
    	String[] s_perturbations = parameters.split(",");
    	if (s_perturbations.length == 1) {
    		return getSimplePerturbationFromString(model, parameters);
    	}
    	
    	List<LogicalModelPerturbation> perturbations = new ArrayList<LogicalModelPerturbation>();
    	for (String s: s_perturbations) {
    		LogicalModelPerturbation p = getSimplePerturbationFromString(model, s);
    		if (p != null) {
    			perturbations.add(p);
    		}
    	}
    	
    	if (perturbations.size() < 1) {
    		return null;
    	}
    	if (perturbations.size() == 1) {
    		return perturbations.get(0);
    	}
    	return new MultiplePerturbation<LogicalModelPerturbation>(perturbations);
    }

    /**
     * Parse and reconstruct a simple perturbation.
     * 
     * @param model
     * @param parameters
     * @return
     */
    private static LogicalModelPerturbation getSimplePerturbationFromString(LogicalModel model, String parameters) {
    	String[] params = parameters.split("%");
    	if (params.length != 2) {
    		//TODO: report error?
    		return null;
    	}
    	String s_source = null;
    	String s_target = params[0];
    	int idx = s_target.indexOf(':');
    	if (idx>=0) {
    		s_source = s_target.substring(0, idx);
    		s_target = s_target.substring(idx+1);
    	}
    	NodeInfo source = model.getComponent(s_source);
    	NodeInfo target = model.getComponent(s_target);

    	String restriction = params[1];
    	
    	idx = restriction.indexOf(':');
    	int min = -1;
    	int max = -1;
    	if (idx >= 0) {
    		min = Integer.parseInt(restriction.substring(0, idx));
    		max = Integer.parseInt(restriction.substring(idx+1));
    	} else {
    		min = Integer.parseInt(restriction);
    		max = min;
    	}
    	
    	if (max < min) {
    		min = max;
    	}
    	
    	if (source == null) {
    		if (min == max) {
    			return new FixedValuePerturbation(target, min);
    		}
    		return new RangePerturbation(target, min, max);
    	}
    	
    	if (min == max) {
    		return new InteractionPerturbation(source, target, min);
    	}
    	
    	// TODO: support range restriction on interactions?
    	return null;
    }
}
