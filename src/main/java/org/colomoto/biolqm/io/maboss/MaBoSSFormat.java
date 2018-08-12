package org.colomoto.biolqm.io.maboss;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class MaBoSSFormat extends AbstractFormat {

	public MaBoSSFormat() { super("bnd", "MaBoSS format", MultivaluedSupport.BOOLEANIZED); }

	@Override
	public MaBoSSEncoder getExporter(LogicalModel model, StreamProvider streams) {
		return new MaBoSSEncoder(model, streams);
	}

}
