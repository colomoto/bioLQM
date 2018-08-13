package org.colomoto.biolqm.io.bnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class BNetFormat extends AbstractFormat {

	public static final String ID = "bnet";

	public BNetFormat() {
		super(ID, "bnet functions format", MultivaluedSupport.BOOLEANIZED);
		
	}

	@Override
	public BNetImport getLoader() {
		return new BNetImport();
	}

	@Override
	public BNetExport getExporter(LogicalModel model) {
		return new BNetExport(model);
	}

}
