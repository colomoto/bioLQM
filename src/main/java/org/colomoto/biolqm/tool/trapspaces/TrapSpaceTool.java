package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.colomoto.common.task.Task;
import org.mangosdk.spi.ProviderFor;


@ProviderFor(LogicalModelTool.class)
public class TrapSpaceTool extends AbstractTool<TrapSpaceList, TrapSpaceSettings> {

	public static final String HELP_LINE = "Search trap spaces using ASP or BDDs";
	public static final String HELP_MESSAGE = "arguments: (all,percolate) ; (BDD,ASP,showASP) ; (raw,terminal,diag)";
	
	public TrapSpaceTool() {
		super("trapspace", HELP_LINE, HELP_MESSAGE, true);
	}

	@Override
	public void run(LogicalModel model, String ... parameters) {
		TrapSpaceSettings settings = getSettings(model, parameters);
		TrapSpaceIdentifier identifier = new TrapSpaceIdentifier(settings);
		identifier.runCLI();
	}
	
	public TrapSpaceSettings getSettings(LogicalModel model, String[] parameters) {
		TrapSpaceSettings settings = new TrapSpaceSettings(model);
		if (parameters != null && parameters.length > 0) {
			boolean focus = false;
			for (String s: parameters) {
				if (focus) {
					settings.focusComponents = s.split(",");
					focus = false;
				} else if ("terminal".equalsIgnoreCase(s)) {
					settings.terminal = true;
					settings.diag = false;
				} else if ("raw".equalsIgnoreCase(s)) {
					settings.terminal = false;
					settings.diag = false;
				} else if ("diag".equalsIgnoreCase(s) || "tree".equalsIgnoreCase(s)) {
					settings.terminal = false;
					settings.diag = true;
					
				} else if ("percolate".equalsIgnoreCase(s)) {
					settings.percolate = true;
				} else if ("all".equalsIgnoreCase(s)) {
					settings.percolate = false;
					
				} else if ("reduce".equalsIgnoreCase(s)) {
					settings.reduce = true;

				} else if ("bdd".equalsIgnoreCase(s)) {
					settings.bdd = true;
				} else if ("asp".equalsIgnoreCase(s)) {
					settings.bdd = false;
				} else if ("showASP".equalsIgnoreCase(s)) {
					settings.bdd = false;
					settings.showasp = true;
				} else if ("focus".equalsIgnoreCase(s)) {
					focus = true;
				} else {
					System.out.println("Unknown parameter: "+ s);
				}
			}
		}
		return settings;
	}
	
	@Override
	public TrapSpaceList getResult(TrapSpaceSettings settings) throws Exception {
		return getTask(settings).call();
	}

	public Task<TrapSpaceList> getTask(TrapSpaceSettings settings) {
		return new TrapSpaceIdentifier(settings);
	}

}
