package org.colomoto.biolqm.io.mnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

import java.io.*;

@MetaInfServices(LogicalModelFormat.class)
public class MNetFormat extends AbstractFormat {

	public static final String ID = "mnet";

	public MNetFormat() {
		super(ID, "Multi-valued functions format", MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public LogicalModel load(InputStreamProvider ip) throws IOException {
		Reader reader = new InputStreamReader( ip.getInputStream());
		return MNetImport.getModel(reader);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		MNetExport exporter = new MNetExport();
		exporter.export(model, out);
	}
}
