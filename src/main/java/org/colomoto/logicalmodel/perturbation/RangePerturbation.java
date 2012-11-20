package org.colomoto.logicalmodel.perturbation;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;

/**
 * Simple perturbation to fix the value of a component.
 * 
 * @author Aurelien Naldi
 */
public class RangePerturbation extends AbstractPerturbation {

	public final int min, max;
	public final NodeInfo component;

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
	public RangePerturbation(NodeInfo target, int min, int max) {
		if (min < 0 || max < min || max > target.getMax()) {
			throw new RuntimeException("Invalid perturbation range for "+target+": "+min+","+max);
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
			RangeRestrictionOperation op = new RangeRestrictionOperation(model.getMDDManager(), min, max);
			functions[idx] = op.restrict(oldValue);
		}
		
		model.getMDDManager().free(oldValue);
	}
	
	public String toString() {
		return component.getNodeID() + " ["+min+","+max+"]";
	}
	
	public boolean equals(Object o) {
		if (o instanceof RangePerturbation) {
			RangePerturbation p = (RangePerturbation)o;
			return p.min == this.min && p.max == this.max && p.component.equals(this.component); 
		}
		return false;
	}

}
