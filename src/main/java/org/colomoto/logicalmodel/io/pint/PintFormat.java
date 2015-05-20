package org.colomoto.logicalmodel.io.pint;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelFormat.class)
public class PintFormat extends AbstractFormat {

	public PintFormat() { super("an", "Pint format", ModelType.MULTIVALUED); }

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

