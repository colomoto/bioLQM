package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.helper.implicants.Formula;

public interface TrapSpaceSolver {

	void add_variable(int idx, Formula formula, Formula not_formula);

	void add_fixed(int idx, int value);
	
	void solve(TrapSpaceList solutions);

	void add_focus(int idx);

	default String show() {
		throw new UnsupportedOperationException();
	}
}
