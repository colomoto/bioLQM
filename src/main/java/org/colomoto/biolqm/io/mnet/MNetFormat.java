package org.colomoto.biolqm.io.mnet;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.*;

@MetaInfServices(LogicalModelFormat.class)
public class MNetFormat extends AbstractFormat {

	public static final String ID = "mnet";

	public MNetFormat() {
		super(ID, "Multi-valued functions format", MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public LogicalModel load(StreamProvider streams) throws IOException {
		Reader reader = new InputStreamReader( streams.input());
		return MNetImport.getModel(reader);
	}

	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		MNetExport exporter = new MNetExport();
		exporter.export(model, streams.output());
	}
}
