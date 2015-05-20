package org.colomoto.logicalmodel.io.boolsim;

import java.io.*;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelFormat.class)
public class BoolSimFormat extends AbstractFormat {

	public static final String ID = "boolsim";
	
	public BoolSimFormat() {
		super(ID, "boolsim format", ModelType.BOOLEAN_CAN_EXPORT_MULTIVALUE);
	}

	
	@Override
	public LogicalModel importFile(File f) throws IOException {

		Reader reader = new FileReader( f);
		return BoolSimImport.getModel(reader);
	}


	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		BoolSimExport exporter = new BoolSimExport();
		exporter.export(model, out);
	}

}
