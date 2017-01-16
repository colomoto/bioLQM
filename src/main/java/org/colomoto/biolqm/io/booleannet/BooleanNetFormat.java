package org.colomoto.biolqm.io.booleannet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

import java.io.*;

@ProviderFor(LogicalModelFormat.class)
public class BooleanNetFormat extends AbstractFormat {

	public static final String ID = "booleannet";

	public BooleanNetFormat() {
		super(ID, "Alternative functions format");
	}


	@Override
	public LogicalModel importFile(File f) throws IOException {

		Reader reader = new FileReader( f);
		return BooleanNetImport.getModel(reader);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BooleanNetExport exporter = new BooleanNetExport();
		exporter.export(model, out);
	}

}
