package org.colomoto.biolqm.io.mnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
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
	public LogicalModel importFile(File f) throws IOException {
		Reader reader = new FileReader( f);
		return MNetImport.getModel(reader);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		MNetExport exporter = new MNetExport();
		exporter.export(model, out);
	}
}
