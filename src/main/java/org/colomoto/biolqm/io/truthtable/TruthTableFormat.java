package org.colomoto.biolqm.io.truthtable;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

/**
 * Format description for TruthTable (.tt) files. These files are in a two
 * column format, representing an explicit truth table.
 * 
 * @author Pedro T. Monteiro
 */
@MetaInfServices(LogicalModelFormat.class)
public class TruthTableFormat extends AbstractFormat {

	public TruthTableFormat() {
		super("tt", "Truth table format", MultivaluedSupport.MULTIVALUED);
	}

	@Override
	public TruthTableImport getLoader() {
		return new TruthTableImport();
	}

	@Override
	public TruthTableExport getExporter(LogicalModel model) {
		return new TruthTableExport(model);
	}
}
