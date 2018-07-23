package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.mangosdk.spi.ProviderFor;


@ProviderFor(ModelToolService.class)
public class TrapSpaceService extends AbstractToolService<TrapSpaceList, TrapSpaceTask> {

	public static final String HELP_LINE = "Search trap spaces using ASP or BDDs";
	public static final String HELP_MESSAGE = "arguments: (all,percolate) ; (BDD,ASP,showASP) ; (raw,terminal,diag)";
	
	public TrapSpaceService() {
		super("trapspace", HELP_LINE, HELP_MESSAGE, true);
	}

	@Override
	public void run(LogicalModel model, String ... parameters) {
		TrapSpaceTask task = getTask(model, parameters);
		task.runCLI();
	}

	@Override
	public TrapSpaceTask getTask(LogicalModel model, String[] parameters) {
		return new TrapSpaceTask(model, parameters);
	}
	
}
