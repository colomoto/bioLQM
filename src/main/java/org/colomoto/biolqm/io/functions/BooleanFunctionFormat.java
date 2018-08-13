package org.colomoto.biolqm.io.functions;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class BooleanFunctionFormat extends AbstractFormat {

	public static final String ID = "boolfunctions";
	
	public BooleanFunctionFormat() {
		super(ID, "Raw functions format", MultivaluedSupport.BOOLEANIZED);
	}

	@Override
	public BooleanFunctionImport getLoader() {
		return new BooleanFunctionImport();
	}

	@Override
	public BooleanFunctionExport getExporter(LogicalModel model) {
		return new BooleanFunctionExport(model);
	}

}
