package org.colomoto.biolqm.tool;

import org.colomoto.MultivaluedSupport;
import org.colomoto.biolqm.BaseService;
import org.colomoto.biolqm.LogicalModel;

/**
 * Base class for the boring parts of tool declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractToolService<R, T extends ToolTask<R>> extends BaseService implements ModelToolService<R,T> {

	private final MultivaluedSupport supportsMultivalued;
	
	protected AbstractToolService(String id, String name) {
		this(id, null, name, "", MultivaluedSupport.BOOLEANIZED);
	}

	protected AbstractToolService(String id, String name, String helpMessage, MultivaluedSupport supportsMultivalued) {
		this(id, null, name, helpMessage, supportsMultivalued);
	}

	protected AbstractToolService(String id, String[] aliases, String name, String helpMessage, MultivaluedSupport supportsMultivalued) {
		super(id, aliases, name, helpMessage);
		this.supportsMultivalued = supportsMultivalued;
	}
	
	@Override
	public T getTask(LogicalModel model) {
		return getTask(model, new String[0]);
	}
	
	@Override
	public T getTask(LogicalModel model, String s) {
		return getTask(model, s.split(" "));
	}

	@Override
	public MultivaluedSupport getMultivaluedSupport() {
		return supportsMultivalued;
	}

}
