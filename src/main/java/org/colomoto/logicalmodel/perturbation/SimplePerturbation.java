package org.colomoto.logicalmodel.perturbation;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;

/**
 * Simple perturbation to fix the value of a component.
 * 
 * @author Aurelien Naldi
 */
public class SimplePerturbation extends AbstractPerturbation {

	private final int min, max;
	private final NodeInfo component;

	/**
	 * Create a simple KO perturbation.
	 * 
	 * @param target the blocked component.
	 */
	public SimplePerturbation(NodeInfo target) {
		this(target, 0, 0);
	}
	
	/**
	 * Create a simple perturbation to fix the value of a component.
	 * 
	 * @param target the blocked component
	 * @param value the fixed value
	 */
	public SimplePerturbation(NodeInfo target, int value) {
		this(target, value, value);
	}
	
	/**
	 * Create a range restriction.
	 * Note that the values should verify 0 <= min <= max < nbval.
	 * if min = max, then the component will have a fixed value.
	 * Otherwise, a new function will be computed to ensure that the reached leaf belongs to the [min,max] range.
	 * 
	 * <p>Warning: This range restriction is not yet implemented, this full constructor is thus private for now.
	 * 
	 * @param target
	 * @param min
	 * @param max
	 */
	private SimplePerturbation(NodeInfo target, int min, int max) {
		if (min < 0 || max < min || max > target.getMax()) {
			throw new RuntimeException("Invalid perturbation settings for "+target+": "+min+","+max);
		}
		this.component = target;
		this.min = min;
		this.max = max;
	}
	
	@Override
	public void update(LogicalModel model) {
		int idx = -1;
		int[] functions = null;
		
		idx = model.getNodeOrder().indexOf(component);
		if (idx >= 0) {
			functions = model.getLogicalFunctions();
		} else {
			idx = model.getExtraComponents().indexOf(component);
			if (idx >= 0) {
				functions = model.getExtraLogicalFunctions();
			}
		}
		
		if (idx < 0) {
			throw new RuntimeException("Perturbation.update(): Could not find the target component");
		}
		
		if (min == 0 && max == component.getMax()) {
			// no change here
			return;
		}

		int oldValue = functions[idx];
		if (min == max) {
			functions[idx] = min;
		} else {
			// FIXME: implement range restriction
			throw new RuntimeException("Perturbation.update(): Range restriction not yet implemented");
		}
		
		model.getMDDManager().free(oldValue);
	}
}
