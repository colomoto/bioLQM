package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.kohsuke.MetaInfServices;


@MetaInfServices(ModelToolService.class)
public class TrapSpaceService extends AbstractToolService<TrapSpaceList, TrapSpaceTask> {

	public static final String UID = "trapspaces";
	public static final String[] ALIASES = { "trapspace" };
	public static final String HELP_LINE = "Search trap spaces using ASP or BDDs";
	public static final String HELP_MESSAGE = "arguments: (all,percolate) ; (BDD,ASP,showASP) ; (raw,terminal,diag)";
	
	public TrapSpaceService() {
		super(UID, ALIASES, HELP_LINE, HELP_MESSAGE, MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public TrapSpaceTask getTask(LogicalModel model) {
		return new TrapSpaceTask(model);
	}
	
}
