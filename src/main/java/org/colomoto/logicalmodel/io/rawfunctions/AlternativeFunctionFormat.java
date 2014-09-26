package org.colomoto.logicalmodel.io.rawfunctions;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

@ProviderFor(LogicalModelFormat.class)
public class AlternativeFunctionFormat extends AbstractFormat {

	public static final String ID = "altfunctions";

	public AlternativeFunctionFormat() {
		super(ID, "Alternative functions format");
	}

	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		AlternativeFunctionExport exporter = new AlternativeFunctionExport();
		exporter.export(model, out);
	}

}
