package org.colomoto.logicalmodel.io.ginml;

import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

/**
 * Format description for GINML files.
 * These files are used by the GINsim tool.
 * 
 * @author Aurelien Naldi
 */
@ProviderFor(LogicalModelFormat.class)
public class GINMLFormat extends AbstractFormat {

	public GINMLFormat() {
		super("ginml", "GINML format", true);
	}
	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		new LogicalModel2GINML(model).export(out);
	}

}
