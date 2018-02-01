package org.colomoto.biolqm.io;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;

import org.colomoto.biolqm.LogicalModel;

/**
 * Simple helper to fill in the boring parts of format declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractFormat implements LogicalModelFormat {

	private final String formatID;
	private final String formatName;
	private final String[] aliases;

	private final boolean canExport;
	private final boolean canImport;
	private final MultivaluedSupport modelType;
	
	private static final String NAME_IMPORT = "importFile";
	private static final String NAME_EXPORT = "export";

	protected AbstractFormat(String id, String name) {
		this(id, name, MultivaluedSupport.BOOLEAN_STRICT);
	}

	@Deprecated
	protected AbstractFormat(String id, String name, boolean supportsMultivalued) {
		this(id, name, MultivaluedSupport.MULTIVALUED);
	}

	protected AbstractFormat(String id, String name, MultivaluedSupport modelType) {
		this(id, null, name, modelType);
	}

	protected AbstractFormat(String id, String[] aliases, String name, MultivaluedSupport modelType) {
		this.formatID = id;
		this.aliases = aliases;
		this.formatName = name;
		this.modelType = modelType;

		boolean canImport = false;
		boolean canExport = false;
		Class cl = getClass();
		for (Method m: cl.getMethods()) {
			if (m.getDeclaringClass() != cl) {
				continue;
			}
			if (m.getName() == NAME_IMPORT) {
				canImport = true;
			} else if (m.getName() == NAME_EXPORT) {
				canExport = true;
			}
		}
		this.canImport = canImport;
		this.canExport = canExport;
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
	public String[] getAliases() {
		return aliases;
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
	public MultivaluedSupport getMultivaluedSupport() {
		return this.modelType;
	}

	@Override
	public LogicalModel importFile(File f) throws IOException {
		throw new RuntimeException("Import not implemented for format " + getID());
	}

	@Override
	public void export(LogicalModel model, OutputStreamProvider outputProvider) throws IOException {
		export(model, outputProvider.getOutputStream());
	}

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
