package org.colomoto.biolqm.io.bnet;

import org.colomoto.biolqm.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;

@MetaInfServices(LogicalModelFormat.class)
public class BNetFormat extends AbstractFormat {

	public static final String ID = "bnet";

	public BNetFormat() {
		super(ID, "bnet functions format", MultivaluedSupport.BOOLEANIZED);
		
	}

	@Override
	public LogicalModel loadImpl(InputStreamProvider ip) throws IOException {
		return BNetImport.getModel(ip.getReader());
	}


	@Override
	public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
		BNetExport.export(model, out.getOutputStream());
	}

}
