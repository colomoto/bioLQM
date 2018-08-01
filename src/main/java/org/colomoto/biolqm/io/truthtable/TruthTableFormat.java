package org.colomoto.biolqm.io.truthtable;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

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
	public LogicalModel importFile(File f) throws IOException {
		TruthTableImport importer = new TruthTableImport();

		return importer.getModel(f);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		TruthTableExport tt = new TruthTableExport();
		tt.export(model, out);
	}
}
