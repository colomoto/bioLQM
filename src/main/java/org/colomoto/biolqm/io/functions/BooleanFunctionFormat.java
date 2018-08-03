package org.colomoto.biolqm.io.functions;

import org.colomoto.biolqm.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.*;

@MetaInfServices(LogicalModelFormat.class)
public class BooleanFunctionFormat extends AbstractFormat {

	public static final String ID = "boolfunctions";
	
	public BooleanFunctionFormat() {
		super(ID, "Raw functions format", MultivaluedSupport.BOOLEANIZED);
	}

	@Override
	public LogicalModel loadImpl(InputStreamProvider ip) throws IOException {
		return BooleanFunctionImport.getModel( ip.getReader());
	}

	@Override
	public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
		BooleanFunctionExport.export(model, out.getOutputStream());
	}

}
