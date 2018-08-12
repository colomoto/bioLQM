package org.colomoto.biolqm.io.boolsim;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;

@MetaInfServices(LogicalModelFormat.class)
public class BoolSimFormat extends AbstractFormat {

	public static final String ID = "boolsim";
	
	public BoolSimFormat() {
		super(ID, "boolsim format", MultivaluedSupport.BOOLEANIZED);
	}


	@Override
	public BoolSimImport getLoader(StreamProvider streams) {
		return new BoolSimImport(streams);
	}


	@Override
	public BoolSimExport getExporter(LogicalModel model, StreamProvider streams) {
		return new BoolSimExport(model, streams);
	}

}
