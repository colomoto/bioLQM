package org.colomoto.biolqm.tool.trapspaces;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.service.LQMServiceManager;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.booleanize.BooleanizeService;
import org.colomoto.biolqm.modifier.reduction.ReductionService;
import org.colomoto.biolqm.modifier.reduction.ReductionModifier;
import org.colomoto.biolqm.helper.implicants.Formula;
import org.colomoto.biolqm.helper.implicants.MDD2PrimeImplicants;
import org.colomoto.biolqm.tool.AbstractToolTask;
import org.colomoto.mddlib.MDDManager;

public class TrapSpaceTask extends AbstractToolTask<TrapSpaceList> {

    private static final BooleanizeService boolService = LQMServiceManager.get(BooleanizeService.class);
    private static final ReductionService reduceService = LQMServiceManager.get(ReductionService.class);

	private MDDManager ddmanager;
	private MDD2PrimeImplicants primer;
	private TrapSpaceSolver solver;

	private LogicalModel workModel;

	public boolean reduce = false;

	public boolean percolate = true;
	public boolean bdd = false;
	public boolean altasp = false;
	public boolean showasp = false;

	public boolean terminal = true;
	public boolean diag = false;

	public String[] focusComponents = null;

	private static boolean SMART_DIAG = false;

	public TrapSpaceTask(LogicalModel model) {
		super(model);
		workModel = model;
	}

	public void setParameters(String[] parameters) {
		if (parameters == null) {
		    return;
        }

        boolean focus = false;
        for (String s: parameters) {
            if (focus) {
                focusComponents = s.split(",");
                focus = false;
            } else if ("terminal".equalsIgnoreCase(s)) {
                terminal = true;
                diag = false;
            } else if ("raw".equalsIgnoreCase(s)) {
                terminal = false;
                diag = false;
            } else if ("diag".equalsIgnoreCase(s) || "tree".equalsIgnoreCase(s)) {
                terminal = false;
                diag = true;

            } else if ("percolate".equalsIgnoreCase(s)) {
                percolate = true;
            } else if ("all".equalsIgnoreCase(s)) {
                percolate = false;

            } else if ("reduce".equalsIgnoreCase(s)) {
                reduce = true;

            } else if ("bdd".equalsIgnoreCase(s)) {
                bdd = true;
			} else if ("asp".equalsIgnoreCase(s)) {
				bdd = false;
			} else if ("altasp".equalsIgnoreCase(s)) {
				bdd = false;
				altasp = true;
            } else if ("showASP".equalsIgnoreCase(s)) {
                bdd = false;
                showasp = true;
            } else if ("focus".equalsIgnoreCase(s)) {
                focus = true;
            } else {
                System.out.println("Unknown parameter: "+ s);
            }
        }
	}

	public void loadSettings() throws Exception {
		// Ensure that the model is booleanized
        if (!workModel.isBoolean()) {
        	workModel = boolService.modify(workModel);
        }

        if (reduce) {
	        // reduce boring fixed components
	        ReductionModifier reducer = reduceService.getModifier(workModel);
			reducer.handleFixed = true;
			reducer.purgeFixed = true;
			reducer.handleOutputs = false;
			workModel = reducer.call();
        }

        this.ddmanager = workModel.getMDDManager();
        this.primer = new MDD2PrimeImplicants(ddmanager);
        if (bdd) {
            this.solver = new TrapSpaceSolverBDD(workModel, this);
		} else if (altasp) {
			this.solver = new TrapSpaceSolverFunctionASP(workModel, this);
		} else {
			this.solver = new TrapSpaceSolverASP(workModel, this);
        }
	}

	public void loadModel() {
		int[] functions = workModel.getLogicalFunctions();
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

		if (focusComponents != null) {
			for (String sid: focusComponents) {
				int idx=0;
				for (NodeInfo ni: workModel.getComponents()) {
					if (ni.getNodeID().equals(sid)) {
						solver.add_focus(idx);
						break;
					}
					idx++;
				}
			}
		}
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

	@Override
	public void cli() {
		if (showasp) {
			loadModel();
			System.out.println( ((TrapSpaceSolverASP)solver).getASP() );
			return;
		}

		TrapSpaceList solutions = null;
		try {
			solutions = performTask();
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}

		if (diag) {
			int n = solutions.size();
			int k = (int)Math.log10(n) + 1;

			if (SMART_DIAG) {
				List<Integer>[] diagram = solutions.getInclusionDiagram();
		        for (int i=0 ; i<n ; i++) {
		        	TrapSpace s = solutions.get(i);
		        	List<Integer> children = diagram[i];
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

	        boolean[][] diagram = solutions.inclusion();
	        for (int i=0 ; i<n ; i++) {
	        	TrapSpace s = solutions.get(i);
	        	String incl = " ";
	        	for (int j=0 ; j<n ; j++) {
	        		if (diagram[i][j]) {
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

		for (NodeInfo ni: solutions.nodes) {
			System.out.print(ni+" ");
		}
		System.out.println();
        for (TrapSpace s: solutions) {
        	System.out.println(s);
        }
	}

	@Override
	protected TrapSpaceList performTask() throws Exception {
		loadSettings();
		loadModel();
        TrapSpaceList solutions = new TrapSpaceList(this, workModel);
        solver.solve(solutions);

        return solutions;
	}

}
