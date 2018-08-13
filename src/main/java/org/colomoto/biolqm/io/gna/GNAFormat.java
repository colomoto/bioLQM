package org.colomoto.biolqm.io.gna;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

/**
 * Format description for GNA (non-xml) files.
 * These files are used by the Genetic Network Analyzer (GNA) tool.
 * 
 * @author Pedro T. Monteiro
 */
@MetaInfServices(LogicalModelFormat.class)
public class GNAFormat extends AbstractFormat {

	public GNAFormat() {
		super("gna", "GNA (non-xml) format", MultivaluedSupport.MULTIVALUED);
	}
	
	@Override
	public GNAExport getExporter(LogicalModel model) {
		return new GNAExport(model);
	}

}
