package org.colomoto.logicalmodel.io.sbml;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import javax.xml.stream.XMLStreamException;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

/**
 * Format description for SBML files, using the qual extension.
 * 
 * @author Aurelien Naldi
 */
@ProviderFor(LogicalModelFormat.class)
public class SBMLFormat extends AbstractFormat {

	public SBMLFormat() {
		super("sbml", "SBML-qual v1.0 format", true, true, true);
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
