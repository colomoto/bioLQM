package org.colomoto.biolqm.io.sbml;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
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
	public LogicalModel load(StreamProvider streams) throws IOException {
		try {
			return new SBMLqualImport(streams.input()).getModel();
		} catch (XMLStreamException e) {
			throw new IOException(e);
		}
	}

	
	@Override
	public void export(LogicalModel model, StreamProvider streams) throws IOException {
		try {
			new SBMLqualExport(model).export(streams.output());
		} catch (XMLStreamException e) {
			throw new IOException(e);
		}
	}

}
