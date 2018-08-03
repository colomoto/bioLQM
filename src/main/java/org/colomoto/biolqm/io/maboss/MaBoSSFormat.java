package org.colomoto.biolqm.io.maboss;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

import java.io.IOException;
import java.io.OutputStreamWriter;

@MetaInfServices(LogicalModelFormat.class)
public class MaBoSSFormat extends AbstractFormat {

	public MaBoSSFormat() { super("bnd", "MaBoSS format", MultivaluedSupport.BOOLEANIZED); }

	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		MaBoSSEncoder encoder = new MaBoSSEncoder(model);
		
		OutputStreamWriter writer = new OutputStreamWriter(streams.output());
		encoder.write(writer);
		writer.close();

		writer = new OutputStreamWriter(streams.output("$f.cfg"));
		encoder.writeConfig(writer);
		writer.close();
	}
}

