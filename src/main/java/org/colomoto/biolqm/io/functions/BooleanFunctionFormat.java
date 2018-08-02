package org.colomoto.biolqm.io.functions;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

import java.io.*;

@MetaInfServices(LogicalModelFormat.class)
public class BooleanFunctionFormat extends AbstractFormat {

	public static final String ID = "boolfunctions";
	
	public BooleanFunctionFormat() {
		super(ID, "Raw functions format", MultivaluedSupport.BOOLEANIZED);
	}


	@Override
	public LogicalModel load(InputStreamProvider ip) throws IOException {
		Reader reader = new InputStreamReader( ip.getInputStream());
		return BooleanFunctionImport.getModel( reader);
	}


	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BooleanFunctionExport exporter = new BooleanFunctionExport();
		exporter.export(model, out);
	}

}
