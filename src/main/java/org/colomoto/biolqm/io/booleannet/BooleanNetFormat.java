package org.colomoto.biolqm.io.booleannet;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;

@MetaInfServices(LogicalModelFormat.class)
public class BooleanNetFormat extends AbstractFormat {

	public static final String ID = "booleannet";

	public BooleanNetFormat() {
		super(ID, "Alternative functions format", MultivaluedSupport.BOOLEANIZED);
	}


	@Override
	public LogicalModel load(StreamProvider streams) throws IOException {
		return BooleanNetImport.getModel( streams.reader());
	}

	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		BooleanNetExport.export(model, streams.output());
	}

}
