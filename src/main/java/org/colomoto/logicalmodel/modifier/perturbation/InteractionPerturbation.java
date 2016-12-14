package org.colomoto.logicalmodel.modifier.perturbation;

import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * A perturbation which removes the effect of a component on another (i.e. removes an interaction).
 * The target component will behave as if this regulator had a fixed value.
 * 
 * @author Aurelien Naldi
 */
public class InteractionPerturbation extends AbstractPerturbation {

	public final int regValue;
	public final NodeInfo target;
	public final NodeInfo regulator;

	public InteractionPerturbation(NodeInfo regulator, NodeInfo target, int regValue) {
		this.regValue = regValue;
		this.target = target;
		this.regulator = regulator;
	}

	@Override
	public void restrictValues(byte[] state, List<NodeInfo> nodeOrder) {
		// Does not make sense with this perturbation
	}
	
	@Override
	public void update(LogicalModel model) {
		int idx = -1;
		int[] functions = null;
		
		idx = model.getNodeOrder().indexOf(target);
		if (idx >= 0) {
			functions = model.getLogicalFunctions();
		} else {
			idx = model.getExtraComponents().indexOf(target);
			if (idx >= 0) {
				functions = model.getExtraLogicalFunctions();
			}
		}
		
		if (idx < 0) {
			throw new RuntimeException("Perturbation.update(): Could not find the target component");
		}
		
		int oldValue = functions[idx];
		MDDManager manager = model.getMDDManager();
		MDDVariable var = manager.getVariableForKey(regulator);
		RegulatorRemovalOperation op = new RegulatorRemovalOperation(model.getMDDManager(), var, regValue);
		functions[idx] = op.restrict(oldValue);
		
		model.getMDDManager().free(oldValue);
	}

	public String toString() {
		return target.getNodeID() + " ["+ regulator.getNodeID() + "@"+regValue+"]";
	}
	
	public boolean equals(Object o) {
		if (o instanceof InteractionPerturbation) {
			InteractionPerturbation p = (InteractionPerturbation)o;
			return p.regulator.equals(this.regulator) && p.target.equals(this.target) && p.regValue == this.regValue; 
		}
		return false;
	}

    @Override
    public boolean affectsNode(NodeInfo node) {
        return target.equals(node);
    }

}
