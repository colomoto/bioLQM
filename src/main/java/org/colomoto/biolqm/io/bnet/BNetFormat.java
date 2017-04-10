package org.colomoto.biolqm.io.bnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

import java.io.*;

@ProviderFor(LogicalModelFormat.class)
public class BNetFormat extends AbstractFormat {

	public static final String ID = "bnet";

	public BNetFormat() {
		super(ID, "bnet functions format");
	}


	@Override
	public LogicalModel importFile(File f) throws IOException {

		Reader reader = new FileReader( f);
		return BNetImport.getModel(reader);
	}


	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BNetExport exporter = new BNetExport();
		exporter.export(model, out);
	}

}
