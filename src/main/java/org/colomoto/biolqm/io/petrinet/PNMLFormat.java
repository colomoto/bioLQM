package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.mangosdk.spi.ProviderFor;

import java.io.IOException;

/**
 * PNML is an xml-based format for Petri net models.
 *
 */
@ProviderFor(LogicalModelFormat.class)
public class PNMLFormat extends AbstractFormat {

    public PNMLFormat() { super("pnml", "PNML format", MultivaluedSupport.MULTIVALUED); }

    @Override
    public void export(LogicalModel model, OutputStreamProvider out) throws IOException {
        AbstractPNEncoder encoder = PetriNetSubformats.PNML.getEncoder(model);
        PNConfig config = new PNConfig();
        encoder.export(config, out);
    }

}
