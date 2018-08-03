package org.colomoto.biolqm.io.ginml;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;

/**
 * Format description for GINML files.
 * These files are used by the GINsim tool.
 * 
 * @author Aurelien Naldi
 */
@MetaInfServices(LogicalModelFormat.class)
public class GINMLFormat extends AbstractFormat {

	public GINMLFormat() {
		super("ginml", "GINML format", MultivaluedSupport.MULTIVALUED);
	}
	
	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		new LogicalModel2GINML(model).export(streams.output());
	}

}
