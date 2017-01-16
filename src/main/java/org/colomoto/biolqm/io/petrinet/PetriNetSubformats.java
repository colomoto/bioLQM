package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.LogicalModel;

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
	
	public String getExtension() {
		switch (this) {
		case INA:
			return "pnt";
		
		case APNN:
		case PNML:
		default:
			return name().toLowerCase();
		}
	}
}