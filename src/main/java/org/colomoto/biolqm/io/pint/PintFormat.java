package org.colomoto.biolqm.io.pint;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;

@MetaInfServices(LogicalModelFormat.class)
public class PintFormat extends AbstractFormat {

	public PintFormat() { super("an", "Pint format", MultivaluedSupport.MULTIVALUED); }

	@Override
	public void export(LogicalModel model, StreamProvider out) throws IOException {
		PintExport.export(model, out.output());
	}
}

