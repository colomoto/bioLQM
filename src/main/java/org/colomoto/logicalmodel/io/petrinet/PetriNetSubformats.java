package org.colomoto.logicalmodel.io.petrinet;

import org.colomoto.logicalmodel.LogicalModel;

public enum PetriNetSubformats {
	INA, PNML, APNN;

	public AbstractPNEncoder getEncoder(LogicalModel model) {
		switch (this) {
		case INA:
			return new PNEncoderINA(model);
		case APNN:
			return new PNEncoderAPNN(model);
		case PNML:
			return new PNEncoderPNML(model);
		}
		
		throw new RuntimeException("Unrecognized subformat for Petri net export");
	}
}