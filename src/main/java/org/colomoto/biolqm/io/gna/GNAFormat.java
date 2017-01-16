package org.colomoto.biolqm.io.gna;

import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

/**
 * Format description for GNA (non-xml) files.
 * These files are used by the Genetic Network Analyzer (GNA) tool.
 * 
 * @author Pedro T. Monteiro
 */
@ProviderFor(LogicalModelFormat.class)
public class GNAFormat extends AbstractFormat {

	public GNAFormat() {
		super("gna", "GNA (non-xml) format", MultivaluedSupport.MULTIVALUED);
	}
	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		GNAExport gna = new GNAExport();
		gna.export(model, out);
	}

}
