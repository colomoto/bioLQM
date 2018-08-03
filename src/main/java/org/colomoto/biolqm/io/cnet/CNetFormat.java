package org.colomoto.biolqm.io.cnet;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;

@MetaInfServices(LogicalModelFormat.class)
public class CNetFormat extends AbstractFormat {

	public static final String ID = "cnet";

	public CNetFormat() {
		super(ID, "cnet functions format", MultivaluedSupport.BOOLEANIZED);
	}

	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		CNetExport.export(model, streams.output());
	}

}
