package org.colomoto.logicalmodel.io.functions;

import java.io.*;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelFormat.class)
public class BooleanFunctionFormat extends AbstractFormat {

	public static final String ID = "boolfunctions";
	
	public BooleanFunctionFormat() {
		super(ID, "Raw functions format");
	}

	
	@Override
	public LogicalModel importFile(File f) throws IOException {

		Reader reader = new FileReader( f);
		return BooleanFunctionImport.getModel( reader);
	}


	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BooleanFunctionExport exporter = new BooleanFunctionExport();
		exporter.export(model, out);
	}

}
