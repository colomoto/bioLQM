package org.colomoto.logicalmodel.io.booleannet;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

import java.io.IOException;
import java.io.OutputStream;

@ProviderFor(LogicalModelFormat.class)
public class BoolnetFormat extends AbstractFormat {

	public static final String ID = "altfunctions";

	public BoolnetFormat() {
		super(ID, "Alternative functions format");
	}

	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BoolnetExport exporter = new BoolnetExport();
		exporter.export(model, out);
	}

}
