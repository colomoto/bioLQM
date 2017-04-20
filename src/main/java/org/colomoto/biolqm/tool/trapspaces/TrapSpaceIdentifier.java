package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LQMServiceManager;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.modifier.booleanize.ModelBooleanizerService;
import org.colomoto.biolqm.modifier.reduction.ModelReductionService;
import org.colomoto.biolqm.modifier.reduction.ReductionSettings;
import org.colomoto.biolqm.tool.implicants.Formula;
import org.colomoto.biolqm.tool.implicants.MDD2PrimeImplicants;
import org.colomoto.mddlib.MDDManager;

public class TrapSpaceIdentifier {

    private static final ModelBooleanizerService boolService = LQMServiceManager.getModifier(ModelBooleanizerService.class);
    private static final ModelReductionService reduceService = LQMServiceManager.getModifier(ModelReductionService.class);

    private boolean reduce = false;
	private final LogicalModel model;
	private final MDDManager ddmanager;
	private final MDD2PrimeImplicants primer;
	private TrapSpaceSolver solver;

	public TrapSpaceIdentifier(LogicalModel model, boolean bdd) {
		// Ensure that the model is booleanized
        if (!model.isBoolean()) {
        	model = boolService.getModifiedModel(model);
        }
        
        if (reduce) {
	        // reduce boring fixed components
	        ReductionSettings settings = reduceService.getSettings();
	        settings.handleFixed = true;
	        settings.purgeFixed = true;
	        settings.handleOutputs = false;
	    	model = reduceService.getModifier(model, settings).getModifiedModel();
        }
    	this.model = model;
        
        this.ddmanager = model.getMDDManager();
        this.primer = new MDD2PrimeImplicants(ddmanager);
        if (bdd) {
            this.solver = new TrapSpaceSolverBDD(model);
        } else {
        	this.solver = new TrapSpaceSolverASP(model);
        }
	}
	
	public void run() {
		int[] functions = model.getLogicalFunctions();
        for (int i=0 ; i<functions.length ; i++ ) {
        	int f = functions[i];
        	if (f < ddmanager.getLeafCount()) {
        		// TODO: handle fixed nodes
        		continue;
        	}
        	Formula formula = primer.getPrimes(f, 1);
        	Formula not_formula = formula.negatePrimes();
//        	Formula not_formula2 = primer.getPrimes(f, 0);
//        	if (!not_formula.equals(not_formula2)) {
//        		System.err.println("negative prime problem\n"+ not_formula + not_formula2);
//        	}
        	solver.add_variable(i, formula, not_formula);
        }
        
        solver.solve();
	}

}
