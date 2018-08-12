package org.colomoto.biolqm.io.functions;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.*;

@MetaInfServices(LogicalModelFormat.class)
public class BooleanFunctionFormat extends AbstractFormat {

	public static final String ID = "boolfunctions";
	
	public BooleanFunctionFormat() {
		super(ID, "Raw functions format", MultivaluedSupport.BOOLEANIZED);
	}

	@Override
	public BooleanFunctionImport getLoader(StreamProvider streams) {
		return new BooleanFunctionImport( streams);
	}

	@Override
	public BooleanFunctionExport getExporter(LogicalModel model, StreamProvider streams) {
		return new BooleanFunctionExport(model, streams);
	}

}
