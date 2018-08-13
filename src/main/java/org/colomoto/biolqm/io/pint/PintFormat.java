package org.colomoto.biolqm.io.pint;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class PintFormat extends AbstractFormat {

	public PintFormat() { super("an", "Pint format", MultivaluedSupport.MULTIVALUED); }

	@Override
	public PintExport getExporter(LogicalModel model, StreamProvider streams) {
		return new PintExport(model, streams);
	}
}

