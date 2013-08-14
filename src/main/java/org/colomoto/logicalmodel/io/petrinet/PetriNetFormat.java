package org.colomoto.logicalmodel.io.petrinet;

import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormatMultiplexer;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelFormat.class)
public class PetriNetFormat extends AbstractFormatMultiplexer<PetriNetSubformats> {

	public static final String ID = "PN";

	public PetriNetFormat() {
		super(ID, "Petri Net subformats", true, PetriNetSubformats.values());
	}

	@Override
	public void export(LogicalModel model, OutputStream out, PetriNetSubformats subformat) throws IOException {
		
		AbstractPNEncoder encoder = subformat.getEncoder(model);
		PNConfig config = new PNConfig();
		encoder.export(config, out);
	}

}
