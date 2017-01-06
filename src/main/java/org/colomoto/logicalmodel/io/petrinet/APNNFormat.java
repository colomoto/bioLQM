package org.colomoto.logicalmodel.io.petrinet;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.colomoto.logicalmodel.io.OutputStreamProvider;
import org.mangosdk.spi.ProviderFor;

import java.io.IOException;

/**
 * PNML is an xml-based format for Petri net models.
 *
 */
@ProviderFor(LogicalModelFormat.class)
public class APNNFormat extends AbstractFormat {

    public APNNFormat() { super("apnn", "APNN format", MultivaluedSupport.MULTIVALUED); }

    @Override
    public void export(LogicalModel model, OutputStreamProvider out) throws IOException {
        AbstractPNEncoder encoder = PetriNetSubformats.APNN.getEncoder(model);
        PNConfig config = new PNConfig();
        encoder.export(config, out);
    }

}
