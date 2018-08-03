package org.colomoto.biolqm.io;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.service.BaseService;

import java.lang.reflect.Method;

/**
 * Simple helper to fill in the boring parts of format declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractFormat extends BaseService implements LogicalModelFormat {

	private final boolean canExport;
	private final boolean canImport;

	private static final String NAME_IMPORT = "load";
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
		super(id, aliases, name, "", modelType);

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
	public boolean canLoad() {
		return canImport;
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
