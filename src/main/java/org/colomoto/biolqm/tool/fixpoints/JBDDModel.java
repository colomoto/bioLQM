package org.colomoto.biolqm.tool.fixpoints;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDFactory;

public class JBDDModel {

	private final LogicalModel _ori;
	
	private final int nvar;
	private final BDDFactory jbdd;
	private final BDD[] functions;
	
	public JBDDModel(LogicalModel lm) {
		this._ori = lm;
        MDDManager ddmanager = lm.getMDDManager();
		MDDVariable[] variables = ddmanager.getAllVariables();
        nvar = variables.length;
        functions = new BDD[nvar];
        jbdd = BDDFactory.init("java", 100000, 1000);
        jbdd.setVarNum(nvar);

		PathSearcher searcher = new PathSearcher(ddmanager);
        
		int[] lf = lm.getLogicalFunctions();
		
		for (int idx=0 ; idx<lf.length ; idx++) {
			BDD func = jbdd.zero();
			int[] path = searcher.setNode(lf[idx]);
			for (int leaf: searcher) {
				if (leaf == 0) {
					continue;
				}

				// reconstruct a BDD for each path
				BDD cur = jbdd.one();
				for (int i=0 ; i<path.length ; i++) {
					int cst = path[i];
					if (cst < 0) {
						continue;
					}
					BDD ith;
					if (cst == 0) {
						ith = jbdd.nithVar(i);
					} else {
						ith = jbdd.ithVar(i);
					}
					cur.andWith( ith );
				}
				func.orWith(cur);
			}
			functions[idx] = func;
		}
	}

	public static FixpointList getBDD(LogicalModel model) {
		JBDDModel jm = new JBDDModel(model);
		List<NodeInfo> components = model.getComponents();
		BDD stable = jm.stable();
		FixpointList result = new FixpointList(model);
		List<byte[]> l = stable.allsat();
		for (byte[] b: l) {
			// TODO: wrap it in the standard data structure
			for (int idx=0 ; idx<components.size() ; idx++) {
				if (b[idx] < 0) {
					System.out.print("-");
				} else {
					System.out.print(b[idx]);
				}
			}
			System.out.println();
		}

		return result;
	}



	public void print() {
		for (int idx=0 ; idx<functions.length ; idx++) {
			BDD func = functions[idx];
			if (func.isZero()) {
				System.out.println(idx + " = 0");
				continue;
			}
			System.out.println(idx + " = " + func);
		}
	}
	
	public BDD[] biimps() {
		BDD[] biimps = new BDD[nvar];
        for (int idx=0 ; idx<nvar ; idx++) {
        	BDD ith = jbdd.ithVar(idx);
        	BDD ithNext = jbdd.nithVar(nvar+idx);
        	biimps[idx] = ith.biimpWith(ithNext);
        }
		return biimps;
	}
	
	public BDD[] globalFunctions() {
		BDD[] globals = new BDD[nvar];
        jbdd.setVarNum(nvar*2);

        for (int idx=0 ; idx<nvar ; idx++) {
        	BDD ith = jbdd.ithVar(nvar+idx);
        	BDD nith = jbdd.nithVar(nvar+idx);
        	globals[idx] = functions[idx].ite(ith, nith);
        }
        
		return globals;
	}

	public BDD[] globalAsyncFunctions() {
		BDD[] biimps = biimps();
		BDD[] globals = new BDD[nvar];
        jbdd.setVarNum(nvar*2);

        for (int idx=0 ; idx<nvar ; idx++) {
        	BDD ith = jbdd.ithVar(nvar+idx);
        	BDD cur = ith.biimp(functions[idx]);
        	for (int j=0 ; j<nvar ; j++) {
        		if (j == idx) continue;
        		cur.and(biimps[j]);
        	}
        	globals[idx] = cur;
        }
        
		return globals;
	}
	
	public BDD globalSynchronousFunction() {
		BDD[] globals = this.globalFunctions();
		BDD result = jbdd.one();
        Iterable<Integer> ordering = new StructuralNodeOrderer(_ori);
		for (int i: ordering) {
			result.and(globals[i]);
		}
		return result;
	}

	public BDD globalAsynchronousFunction() {
		BDD[] globals = this.globalAsyncFunctions();
		BDD result = jbdd.zero();
        Iterable<Integer> ordering = new StructuralNodeOrderer(_ori);
		for (int i: ordering) {
			result.or(globals[i]);
		}
		return result;
	}

	public BDD globalStable() {
		BDD gfunc = globalFunctions()[0];
		
        for (int idx=0 ; idx<nvar ; idx++) {
        	BDD ith = jbdd.ithVar(idx); 
        	BDD ithNext = jbdd.ithVar(nvar+idx);
        	BDD nith = jbdd.nithVar(idx); 
        	BDD nithNext = jbdd.nithVar(nvar+idx);
        	
        	BDD cur = ith.andWith(ithNext).orWith( nith.andWith(nithNext));
        	gfunc.andWith(cur);
        }
		
		return gfunc;
	}
	
	public BDD stable() {
		BDD result = jbdd.one();
		
        Iterable<Integer> ordering = new StructuralNodeOrderer(_ori);

		for (int i: ordering) {
			BDD ith = jbdd.ithVar(i);
			BDD nith = jbdd.nithVar(i);
			BDD cur = functions[i];

//			BDD curstable = ith.and(cur);
//			curstable.orWith( nith.andWith(ncur));
			BDD curstable = cur.ite(ith, nith);
			result.andWith(curstable);
		}
		return result;
	}
	
    public List<int[]> bddPaths(BDD node) {
        BDDFactory f = node.getFactory();
        int[] set = new int[f.varNum()];
        for (int i=0 ; i<set.length ; i++) {
        	set[i] = -1;
        }
        List<int[]> paths = new ArrayList<int[]>();
        bdd_printset_rec(f, paths, node, set);
        return paths;
    }
    
    private static void bdd_printset_rec(BDDFactory f, List<int[]> paths, BDD r, int[] set) {
        if (r.isZero()) {
        	return;
        }
        
        if (r.isOne()) {
            paths.add(set.clone());
            return;
        }
        
        set[f.var2Level(r.var())] = 0;
        BDD rl = r.low();
        bdd_printset_rec(f, paths, rl, set);
        rl.free();

        set[f.var2Level(r.var())] = 1;
        BDD rh = r.high();
        bdd_printset_rec(f, paths, rh, set);
        rh.free();

        set[f.var2Level(r.var())] = -1;
    }

}

