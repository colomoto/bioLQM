package org.colomoto.biolqm.io.boolsim;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

import java.io.*;

@MetaInfServices(LogicalModelFormat.class)
public class BoolSimFormat extends AbstractFormat {

	public static final String ID = "boolsim";
	
	public BoolSimFormat() {
		super(ID, "boolsim format", MultivaluedSupport.BOOLEANIZED);
	}

	
	@Override
	public LogicalModel importFile(File f) throws IOException {

		Reader reader = new FileReader( f);
		return BoolSimImport.getModel(reader);
	}


	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BoolSimExport exporter = new BoolSimExport();
		exporter.export(model, out);
	}

}
