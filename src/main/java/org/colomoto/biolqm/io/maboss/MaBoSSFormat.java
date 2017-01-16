package org.colomoto.biolqm.io.maboss;

import java.io.IOException;
import java.io.OutputStreamWriter;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelFormat.class)
public class MaBoSSFormat extends AbstractFormat {

	public MaBoSSFormat() { super("bnd", "MaBoSS format", MultivaluedSupport.BOOLEANIZED); }

	@Override
	public void export(LogicalModel model, OutputStreamProvider provider) throws IOException {
		MaBoSSEncoder encoder = new MaBoSSEncoder(model);
		
		OutputStreamWriter writer = new OutputStreamWriter(provider.getOutputStream());
		encoder.write(writer);
		writer.close();

		writer = new OutputStreamWriter(provider.getOutputStream("$f.cfg"));
		encoder.writeConfig(writer);
		writer.close();
	}
}

