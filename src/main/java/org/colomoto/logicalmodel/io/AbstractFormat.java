package org.colomoto.logicalmodel.io;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Simple helper to fill in the boring parts of format declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractFormat implements LogicalModelFormat {

	private final String formatID;
	private final String formatName;

	private final boolean canExport;
	private final boolean canImport;
	
	protected AbstractFormat(String id, String name, boolean canExport, boolean canImport) {
		this.formatID = id;
		this.formatName = name;
		this.canExport = canExport;
		this.canImport = canImport;
	}
	
	@Override
	public String getID() {
		return formatID;
	}

	@Override
	public String getName() {
		return formatName;
	}

	@Override
	public boolean canExport() {
		return canExport;
	}

	@Override
	public boolean canImport() {
		return canImport;
	}

	@Override
	public LogicalModel importFile(File f) throws IOException {
		throw new RuntimeException("Import not implemented for format " + getID());
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		throw new RuntimeException("Export not implemented for format " + getID());
	}
	
	@Override
	public String toString() {
		String cap;
		if (canImport) {
			if (canExport) {
				cap = "Import, Export";
			} else {
				cap = "Import";
			}
		} else {
			if (canExport) {
				cap = "Export";
			} else {
				cap = "---";
			}
		}
		return getID() +"\t"+ getName() + "\t" + cap;
	}
	
}
