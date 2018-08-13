package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;

/**
 * Base format for the PN formats
 */
abstract public class PetriNetFormat extends AbstractFormat {

    public PetriNetFormat(String id, String name) { super(id, name); }

    @Override
	abstract public AbstractPNEncoder getExporter(LogicalModel model);
}
