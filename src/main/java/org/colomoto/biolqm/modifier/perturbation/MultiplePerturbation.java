package org.colomoto.biolqm.modifier.perturbation;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

/**
 * A multiple perturbation is a list of perturbations all applied together.
 * 
 * @author Aurelien Naldi
 */
public class MultiplePerturbation<P extends LogicalModelPerturbation> implements LogicalModelPerturbation {

	public final List<P> perturbations;

	/**
	 * Create a multiple perturbation
	 * 
	 * @param perturbations list of perturbations to apply.
	 */
	public MultiplePerturbation(List<P> perturbations) {
		this.perturbations = perturbations;
	}

	@Override
	public void restrictValues(byte[] state, List<NodeInfo> nodeOrder) {
		for (LogicalModelPerturbation perturbation : perturbations) {
			perturbation.restrictValues(state, nodeOrder);
		}
	}

	@Override
	public void update(LogicalModel model) {
		for (LogicalModelPerturbation perturbation: perturbations) {
			perturbation.update(model);
		}
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		boolean first = true;
		for (LogicalModelPerturbation p: perturbations) {
			if (first) {
				first = false;
			} else {
				sb.append(", ");
			}
			sb.append(p.toString());
		}
		return sb.toString();
	}

	@Override
	public 	String getStringRepresentation() {
		StringBuffer sb = new StringBuffer();
		boolean first = true;
		for (LogicalModelPerturbation p: perturbations) {
			if (first) {
				first = false;
			} else {
				sb.append(",");
			}
			sb.append(p.getStringRepresentation());
		}
		return sb.toString();
	}

	public boolean equals(Object o) {
		if (o instanceof MultiplePerturbation) {
			MultiplePerturbation mp = (MultiplePerturbation)o;
			if (this.perturbations.size() == mp.perturbations.size() && 
					this.perturbations.containsAll(mp.perturbations) &&
					mp.perturbations.containsAll(this.perturbations))
				return true;
		}
		return false;
	}

    @Override
    public boolean affectsNode(NodeInfo node) {
        for (LogicalModelPerturbation p: perturbations) {
            if (p.affectsNode(node)) {
                return true;
            }
        }
        return false;
    }

}
