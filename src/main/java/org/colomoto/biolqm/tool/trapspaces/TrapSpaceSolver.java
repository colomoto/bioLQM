package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.tool.implicants.Formula;

public interface TrapSpaceSolver {

	void add_variable(int idx, Formula formula, Formula not_formula);
	
	void solve();
	
}
