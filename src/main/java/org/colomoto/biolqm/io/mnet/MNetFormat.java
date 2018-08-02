package org.colomoto.biolqm.io.mnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.*;

@MetaInfServices(LogicalModelFormat.class)
public class MNetFormat extends AbstractFormat {

	public static final String ID = "mnet";

	public MNetFormat() {
		super(ID, "Multi-valued functions format", MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public LogicalModel loadImpl(InputStreamProvider ip) throws IOException {
		Reader reader = new InputStreamReader( ip.getInputStream());
		return MNetImport.getModel(reader);
	}

	@Override
	public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
		MNetExport exporter = new MNetExport();
		exporter.export(model, out.getOutputStream());
	}
}
