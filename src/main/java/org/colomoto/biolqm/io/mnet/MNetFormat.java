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
	public MNetImport getLoader() {
		return new MNetImport();
	}

	@Override
	public MNetExport getExporter(LogicalModel model) {
		return new MNetExport(model);
	}
}
