package org.colomoto.biolqm.io.boolsim;

import org.colomoto.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
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
	public LogicalModel loadImpl(InputStreamProvider ip) throws IOException {
		return BoolSimImport.getModel(ip.getReader());
	}


	@Override
	public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
		BoolSimExport.export(model, out.getOutputStream());
	}

}
