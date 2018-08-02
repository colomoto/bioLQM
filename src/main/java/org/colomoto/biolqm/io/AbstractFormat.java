package org.colomoto.biolqm.io;

import org.colomoto.biolqm.BaseService;
import org.colomoto.biolqm.LogicalModel;

import java.io.IOException;
import java.lang.reflect.Method;

/**
 * Simple helper to fill in the boring parts of format declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractFormat extends BaseService implements LogicalModelFormat {

	private final boolean canExport;
	private final boolean canImport;
	private final MultivaluedSupport modelType;
	
	private static final String NAME_IMPORT = "loadImpl";
	private static final String NAME_EXPORT = "exportImpl";

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
		super(id, aliases, name, "");
		this.modelType = modelType;

		boolean canImport = false;
		boolean canExport = false;
		Class cl = getClass();
		for (Method m: cl.getMethods()) {
			if (m.getDeclaringClass() != cl) {
				continue;
			}
			if (NAME_IMPORT.equals(m.getName())) {
				canImport = true;
			} else if (NAME_EXPORT.equals(m.getName())) {
				canExport = true;
			}
		}
		this.canImport = canImport;
		this.canExport = canExport;
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
	public LogicalModel load(InputStreamProvider inputProvider) throws IOException {
		LogicalModel model = loadImpl(inputProvider);

		// TODO: common code for side data: layout, annotations...

		inputProvider.close();
		return model;
	}

	public LogicalModel loadImpl(InputStreamProvider inputProvider) throws IOException {
		throw new RuntimeException("Import not implemented for format " + getID());
	}

	@Override
	public void export(LogicalModel model, OutputStreamProvider outputProvider) throws IOException {
		exportImpl(model, outputProvider);

		// TODO: common code for side data: layout, annotations...
		outputProvider.close();
	}

	public void exportImpl(LogicalModel model, OutputStreamProvider outputProvider) throws IOException {
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
