package org.colomoto.biolqm.io.bnet;

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
public class BNetFormat extends AbstractFormat {

	public static final String ID = "bnet";

	public BNetFormat() {
		super(ID, "bnet functions format", MultivaluedSupport.BOOLEANIZED);
		
	}


	@Override
	public LogicalModel load(InputStreamProvider ip) throws IOException {
		Reader reader = new InputStreamReader( ip.getInputStream());
		return BNetImport.getModel(reader);
	}


	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BNetExport exporter = new BNetExport();
		exporter.export(model, out);
	}

}
