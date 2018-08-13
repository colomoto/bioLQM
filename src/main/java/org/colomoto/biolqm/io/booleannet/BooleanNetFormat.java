package org.colomoto.biolqm.io.booleannet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class BooleanNetFormat extends AbstractFormat {

	public static final String ID = "booleannet";

	public BooleanNetFormat() {
		super(ID, "Alternative functions format", MultivaluedSupport.BOOLEANIZED);
	}


	@Override
	public BooleanNetImport getLoader() {
		return new BooleanNetImport();
	}

	@Override
	public BooleanNetExport getExporter(LogicalModel model) {
		return new BooleanNetExport(model);
	}

}
