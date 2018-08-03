package org.colomoto.biolqm.io.bnet;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;

@MetaInfServices(LogicalModelFormat.class)
public class BNetFormat extends AbstractFormat {

	public static final String ID = "bnet";

	public BNetFormat() {
		super(ID, "bnet functions format", MultivaluedSupport.BOOLEANIZED);
		
	}

	@Override
	public LogicalModel load(StreamProvider streams) throws IOException {
		return BNetImport.getModel(streams.reader());
	}


	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		BNetExport.export(model, streams.output());
	}

}
