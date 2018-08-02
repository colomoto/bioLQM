package org.colomoto.biolqm.io.truthtable;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.InputStreamProvider;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.kohsuke.MetaInfServices;

import java.io.*;

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
	public LogicalModel load(InputStreamProvider ip) throws IOException {
		TruthTableImport importer = new TruthTableImport();
		return importer.getModel(ip);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		TruthTableExport tt = new TruthTableExport();
		tt.export(model, out);
	}
}
