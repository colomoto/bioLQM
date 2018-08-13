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
public class INAFormat extends PetriNetFormat {

    public INAFormat() { super("ina", "INA Petri net format"); }

	@Override
	public PNEncoderINA getExporter(LogicalModel model) {
		return new PNEncoderINA(model);
	}
}
