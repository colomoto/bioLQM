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
public class APNNFormat extends PetriNetFormat {

    public APNNFormat() { super("apnn", "APNN format"); }

	@Override
	public PNEncoderAPNN getExporter(LogicalModel model) {
		return new PNEncoderAPNN(model);
	}

}
