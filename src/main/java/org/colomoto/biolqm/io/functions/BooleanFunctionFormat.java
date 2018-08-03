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
	public LogicalModel load(StreamProvider streams) throws IOException {
		return BooleanFunctionImport.getModel( streams.reader());
	}

	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		BooleanFunctionExport.export(model, streams.output());
	}

}
