package org.colomoto.biolqm.io.pint;

import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelFormat.class)
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
	public void export(LogicalModel model, OutputStream out) throws IOException {
		PintExport exporter = new PintExport();
		exporter.export(model, out);
	}
}

