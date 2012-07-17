package org.colomoto.logicalmodel.io.ginml;

import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;

/**
 * Format description for GINML files.
 * These files are used by the GINsim tool.
 * 
 * @author Aurelien Naldi
 */
public class GINMLFormat extends AbstractFormat {

	public GINMLFormat() {
		super("ginml", "GINML", true, false);
	}
	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		new LogicalModel2GINML(model).export(out);
	}

}
