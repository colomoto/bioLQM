package org.colomoto.biolqm.io.boolsim;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;

@MetaInfServices(LogicalModelFormat.class)
public class BoolSimFormat extends AbstractFormat {

	public static final String ID = "boolsim";
	
	public BoolSimFormat() {
		super(ID, "boolsim format", MultivaluedSupport.BOOLEANIZED);
	}


	@Override
	public LogicalModel load(InputStreamProvider ip) throws IOException {
		Reader reader = new InputStreamReader( ip.getInputStream());
		return BoolSimImport.getModel(reader);
	}


	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BoolSimExport exporter = new BoolSimExport();
		exporter.export(model, out);
	}

}
