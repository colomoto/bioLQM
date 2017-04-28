package org.colomoto.biolqm.tool.trapspaces;

import java.util.ArrayList;
import java.util.List;

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
	
	private final TrapSpaceSettings settings;
	
	private static boolean SMART_TREE = false;
	
	public TrapSpaceIdentifier(LogicalModel model, TrapSpaceSettings settings) {
		this.settings = settings;
		
		// Ensure that the model is booleanized
        if (!model.isBoolean()) {
        	model = boolService.getModifiedModel(model);
        }
        
        if (reduce) {
	        // reduce boring fixed components
	        ReductionSettings rsettings = reduceService.getSettings();
	        rsettings.handleFixed = true;
	        rsettings.purgeFixed = true;
	        rsettings.handleOutputs = false;
	    	model = reduceService.getModifier(model, rsettings).getModifiedModel();
        }
    	this.model = model;
        
        this.ddmanager = model.getMDDManager();
        this.primer = new MDD2PrimeImplicants(ddmanager);
        if (settings.bdd) {
            this.solver = new TrapSpaceSolverBDD(model, settings);
        } else {
        	this.solver = new TrapSpaceSolverASP(model, settings);
        }
	}
	
	public void loadModel() {
		int[] functions = model.getLogicalFunctions();
        for (int i=0 ; i<functions.length ; i++ ) {
        	int f = functions[i];
        	if (f < ddmanager.getLeafCount()) {
        		// special handling for fixed nodes
            	solver.add_fixed(i, f);
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
	}

	public TrapSpaceList getSolutions() {
		loadModel();
        TrapSpaceList solutions = new TrapSpaceList(settings);
        solver.solve(solutions);
        
        return solutions;
	}

	public static List<TrapSpace> selectAttractors(List<TrapSpace> solutions) {

		List<TrapSpace> selected = new ArrayList<TrapSpace>();
		int n = solutions.size();
		for (int i=0 ; i<n ; i++) {
			boolean keep = true;
			TrapSpace s1 = solutions.get(i);
			for (int j=0 ; j<n ; j++) {
				if (i==j) {
					continue;
				}
				if (s1.contains(solutions.get(j))) {
					keep = false;
					break;
				}
			}
			if (keep) {
				selected.add(s1);
			}
		}
		return selected;
	}
	
	public void run() {
		if (settings.showasp) {
			loadModel();
			System.out.println( ((TrapSpaceSolverASP)solver).getASP() );
			return;
		}
		
		TrapSpaceList solutions = getSolutions();
		if (settings.tree) {
			int n = solutions.size();
			int k = (int)Math.log10(n) + 1;

			if (SMART_TREE) {
				List<Integer>[] tree = solutions.getInclusiontree();
		        for (int i=0 ; i<n ; i++) {
		        	TrapSpace s = solutions.get(i);
		        	List<Integer> children = tree[i];
		        	String incl = " ";
		        	if (children == null) {
		        		incl = "@";
		        	} else {
		        		for (int c: children) {
		        			incl += " "+c;
		        		}
		        	}
		        	System.out.format("%"+k+"d:  %s   | %s\n", i, s, incl);
		        }
		        
		        System.out.println();
		        System.out.println();
			}
			
	        boolean[][] btree = solutions.inclusion();
	        for (int i=0 ; i<n ; i++) {
	        	TrapSpace s = solutions.get(i);
	        	String incl = " ";
	        	for (int j=0 ; j<n ; j++) {
	        		if (btree[i][j]) {
	        			incl += " "+j;
	        		}
	        	}
	        	if (incl.length() < 2) {
	        		incl = "@";
	        	}
	        	System.out.format("%"+k+"d:  %s   | %s\n", i, s, incl);
	        }
	        return;
		}
		
        for (TrapSpace s: solutions) {
        	System.out.println(s);
        }
	}

}
