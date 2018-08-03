package org.colomoto.biolqm.io.pint;

import org.colomoto.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;
import java.io.OutputStream;

@MetaInfServices(LogicalModelFormat.class)
public class PintFormat extends AbstractFormat {

	public PintFormat() { super("an", "Pint format", MultivaluedSupport.MULTIVALUED); }

	/*
	@Override
	public LogicalModel importFile(File f) throws IOException {
		PintImport importer = new PintImport();
		importer.parse(f);
		return importer.getModel();
	}
	*/

	@Override
	public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
		PintExport.export(model, out.getOutputStream());
	}
}

