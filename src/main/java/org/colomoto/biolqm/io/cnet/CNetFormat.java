package org.colomoto.biolqm.io.cnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

import java.io.*;

@ProviderFor(LogicalModelFormat.class)
public class CNetFormat extends AbstractFormat {

	public static final String ID = "cnet";

	public CNetFormat() {
		super(ID, "cnet functions format", MultivaluedSupport.BOOLEANIZED);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		CNetExport exporter = new CNetExport();
		exporter.export(model, out);
	}

}
