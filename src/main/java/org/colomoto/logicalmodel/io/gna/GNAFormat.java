package org.colomoto.logicalmodel.io.gna;

import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
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
		super("gna", "GNA (non-xml) format", true);
	}
	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		GNAExport gna = new GNAExport();
		gna.export(model, out);
	}

}
