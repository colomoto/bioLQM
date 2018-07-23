package org.colomoto.biolqm.helper.implicants;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.mddlib.MDDManager;

public class MDD2PrimeImplicants {

	
	private final MDDManager ddmanager;
	
	public MDD2PrimeImplicants(MDDManager ddmanager) {
		this.ddmanager = ddmanager;
	}
	
	/**
	 * Extract the prime implicants from a MDD-represented function.
	 * For this we need to start constructing all implicants using only the relevant regulators
	 * A first visit of the MDD is used to extract all regulators
	 * Then paths to TRUE in the space of these regulators are extracted, and transformed into
	 * an exhaustive list of all implicants.
	 * The prime implicants are extracted from this list.
	 * Finally, these prime implicants are converted back into the space with all variables.
	 *
	 * @param f the MDD identifier for the logical function
	 * @param t the target value
	 * @return a Formula representing the prime implicants
	 */
    public Formula getPrimes(int f, int t) {
        ImplicantSearcher isearcher = new ImplicantSearcher(ddmanager, t);
    	byte[] path = isearcher.setNode(f);
        List<Term> implicants = new ArrayList<Term>();
        for (int v: isearcher) {
        	implicants.add(new Term(path.clone()));
        }
        int[] regulators = isearcher.getRegulatorList();
        Formula formula = new Formula(implicants, regulators);
        formula.reduceToPrimeImplicants();
        
        return formula;
    }

}
