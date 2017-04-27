package org.colomoto.biolqm.tool.trapspaces;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.mangosdk.spi.ProviderFor;


@ProviderFor(LogicalModelTool.class)
public class TrapSpaceTool extends AbstractTool {

	public static final String HELP_LINE = "Search trap spaces using ASP or BDDs";
	public static final String HELP_MESSAGE = "arguments: (all,percolate) ; (BDD,ASP,showASP) ; (raw,terminal,tree)";
	
	public TrapSpaceTool() {
		super("trapspace", HELP_LINE, HELP_MESSAGE, true);
	}

	@Override
	public void run(LogicalModel model, String[] parameters) {
		TrapSpaceSettings settings = getSettings(parameters);
		TrapSpaceIdentifier identifier = new TrapSpaceIdentifier(model, settings);
		identifier.run();
	}
	
	public TrapSpaceSettings getSettings(String[] parameters) {
		TrapSpaceSettings settings = new TrapSpaceSettings();
		if (parameters != null && parameters.length > 0) {
			for (String s: parameters) {
				if ("terminal".equalsIgnoreCase(s)) {
					settings.terminal = true;
					settings.tree = false;
				} else if ("raw".equalsIgnoreCase(s)) {
					settings.terminal = false;
					settings.tree = false;
				} else if ("tree".equalsIgnoreCase(s)) {
					settings.terminal = false;
					settings.tree = true;
					
				} else if ("percolate".equalsIgnoreCase(s)) {
					settings.percolate = true;
				} else if ("all".equalsIgnoreCase(s)) {
					settings.percolate = false;
					
				} else if ("bdd".equalsIgnoreCase(s)) {
					settings.bdd = true;
				} else if ("asp".equalsIgnoreCase(s)) {
					settings.bdd = false;
				} else if ("showASP".equalsIgnoreCase(s)) {
					settings.bdd = false;
					settings.showasp = true;
					
				} else {
					System.out.println("Unknown parameter: "+ s);
				}
			}
		}
		return settings;
	}
	
	public List<TrapSpace> getSolutions(LogicalModel model, TrapSpaceSettings settings) {
		TrapSpaceIdentifier identifier = new TrapSpaceIdentifier(model, settings);
		return identifier.getSolutions();
	}
	
}
