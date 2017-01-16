package org.colomoto.biolqm.io.ginml;

import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
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
		super("ginml", "GINML format", MultivaluedSupport.MULTIVALUED);
	}
	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		new LogicalModel2GINML(model).export(out);
	}

}
