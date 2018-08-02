package org.colomoto.biolqm.io.booleannet;

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
public class BooleanNetFormat extends AbstractFormat {

	public static final String ID = "booleannet";

	public BooleanNetFormat() {
		super(ID, "Alternative functions format", MultivaluedSupport.BOOLEANIZED);
	}


	@Override
	public LogicalModel loadImpl(InputStreamProvider ip) throws IOException {
		return BooleanNetImport.getModel( ip.getReader());
	}

	@Override
	public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
		BooleanNetExport.export(model, out.getOutputStream());
	}

}
