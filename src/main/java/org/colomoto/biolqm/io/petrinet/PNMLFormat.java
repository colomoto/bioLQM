package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.kohsuke.MetaInfServices;

/**
 * PNML is an xml-based format for Petri net models.
 *
 */
@MetaInfServices(LogicalModelFormat.class)
public class PNMLFormat extends AbstractFormat {

    public PNMLFormat() { super("pnml", "PNML format"); }

	@Override
	public PNEncoderPNML getExporter(LogicalModel model, StreamProvider streams) {
		return new PNEncoderPNML(model, streams);
	}


}
