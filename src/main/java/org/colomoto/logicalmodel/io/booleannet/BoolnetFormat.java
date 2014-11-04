package org.colomoto.logicalmodel.io.booleannet;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

import java.io.*;

@ProviderFor(LogicalModelFormat.class)
public class BoolnetFormat extends AbstractFormat {

	public static final String ID = "booleannet";

	public BoolnetFormat() {
		super(ID, "Alternative functions format");
	}


	@Override
	public LogicalModel importFile(File f) throws IOException {

		Reader reader = new FileReader( f);
		return BooleanNetParserManager.getModel( reader);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BoolnetExport exporter = new BoolnetExport();
		exporter.export(model, out);
	}

}
