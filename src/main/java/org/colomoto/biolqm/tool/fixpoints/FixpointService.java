package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.tool.AbstractToolService;
import org.colomoto.biolqm.tool.ModelToolService;
import org.kohsuke.MetaInfServices;


@MetaInfServices(ModelToolService.class)
public class FixpointService extends AbstractToolService<FixpointList, FixpointTask> {

	public static final String UID = "fixpoints";
	public static final String[] ALIASES = { "stable", "fixed", "fp" };

	public static final String HELP_LINE = "Search fixed (stable) states";
	public static final String HELP_MESSAGE = "arguments: asp pattern";

	public FixpointService() {
		super(UID, ALIASES, HELP_LINE, HELP_MESSAGE, MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public FixpointTask getTask(LogicalModel model) {
		return new FixpointTask(model);
	}
}
