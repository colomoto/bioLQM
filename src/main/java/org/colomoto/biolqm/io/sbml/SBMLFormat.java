package org.colomoto.biolqm.io.sbml;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

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
	public LogicalModel importFile(File f) throws IOException {
		try {
			return new SBMLqualImport(f).getModel();
		} catch (XMLStreamException e) {
			throw new IOException(e);
		}
	}

	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		try {
			new SBMLqualExport(model).export(out);
		} catch (XMLStreamException e) {
			throw new IOException(e);
		}
	}

}
