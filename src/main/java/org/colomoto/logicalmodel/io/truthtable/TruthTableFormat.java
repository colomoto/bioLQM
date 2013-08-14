package org.colomoto.logicalmodel.io.truthtable;

import java.io.File;
import java.io.IOException;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

/**
 * Format description for TruthTable (.tt) files. These files are in a two
 * column format, representing an explicit truth table.
 * 
 * @author Pedro T. Monteiro
 */
@ProviderFor(LogicalModelFormat.class)
public class TruthTableFormat extends AbstractFormat {

	public TruthTableFormat() {
		super("tt", "Truth table format", true);
	}

	@Override
	public LogicalModel importFile(File f) throws IOException {
		TruthTableImport importer = new TruthTableImport();

		return importer.getModel(f);
	}

}
