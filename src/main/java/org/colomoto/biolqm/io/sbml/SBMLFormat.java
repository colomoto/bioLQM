package org.colomoto.biolqm.io.sbml;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;

/**
 * Format description for SBML files, using the qual extension.
 * 
 * @author Aurelien Naldi
 */
@MetaInfServices(LogicalModelFormat.class)
public class SBMLFormat extends AbstractFormat {

	public SBMLFormat() {
		super("sbml", "SBML-qual v1.0 format", MultivaluedSupport.MULTIVALUED);
	}


	@Override
	public SBMLqualImport getLoader(StreamProvider streams) {
		return new SBMLqualImport(streams);
	}

	@Override
	public SBMLqualExport getExporter(LogicalModel model, StreamProvider streams) {
		return new SBMLqualExport(model, streams);
	}

}
