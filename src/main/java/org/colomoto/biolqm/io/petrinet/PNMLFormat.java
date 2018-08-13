package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

/**
 * PNML is an xml-based format for Petri net models.
 *
 */
@MetaInfServices(LogicalModelFormat.class)
public class PNMLFormat extends PetriNetFormat {

    public PNMLFormat() { super("pnml", "PNML format"); }

	@Override
	public PNEncoderPNML getExporter(LogicalModel model) {
		return new PNEncoderPNML(model);
	}


}
