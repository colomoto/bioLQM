package org.colomoto.biolqm.io.boolsim;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class BoolSimFormat extends AbstractFormat {

	public static final String ID = "boolsim";
	
	public BoolSimFormat() {
		super(ID, "boolsim format", MultivaluedSupport.BOOLEANIZED);
	}


	@Override
	public BoolSimImport getLoader() {
		return new BoolSimImport();
	}


	@Override
	public BoolSimExport getExporter(LogicalModel model) {
		return new BoolSimExport(model);
	}

}
