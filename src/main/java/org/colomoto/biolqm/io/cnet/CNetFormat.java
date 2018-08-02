package org.colomoto.biolqm.io.cnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;
import java.io.OutputStream;

@MetaInfServices(LogicalModelFormat.class)
public class CNetFormat extends AbstractFormat {

	public static final String ID = "cnet";

	public CNetFormat() {
		super(ID, "cnet functions format", MultivaluedSupport.BOOLEANIZED);
	}

	@Override
	public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
		CNetExport.export(model, out.getOutputStream());
	}

}
