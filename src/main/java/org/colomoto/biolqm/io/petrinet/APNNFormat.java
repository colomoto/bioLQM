package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

/**
 * PNML is an xml-based format for Petri net models.
 *
 */
@MetaInfServices(LogicalModelFormat.class)
public class APNNFormat extends PNFormat {

    public APNNFormat() { super("apnn", "APNN format"); }

	@Override
	AbstractPNEncoder getEncoder(LogicalModel model) {
		return new PNEncoderAPNN(model);
	}

}
