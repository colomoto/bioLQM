package org.colomoto.biolqm.tool;

import org.colomoto.biolqm.service.BaseService;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.MultivaluedSupport;

/**
 * Base class for the boring parts of tool declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractToolService<R, T extends ToolTask<R>> extends BaseService implements ModelToolService<R,T> {

	protected AbstractToolService(String id, String name) {
		this(id, null, name, "", MultivaluedSupport.BOOLEANIZED);
	}

	protected AbstractToolService(String id, String name, String helpMessage, MultivaluedSupport supportsMultivalued) {
		this(id, null, name, helpMessage, supportsMultivalued);
	}

	protected AbstractToolService(String id, String[] aliases, String name, String helpMessage, MultivaluedSupport supportsMultivalued) {
		super(id, aliases, name, helpMessage, supportsMultivalued);
	}
	
}
