package org.colomoto.biolqm.tool;

import org.colomoto.biolqm.LogicalModel;

/**
 * Base class for the boring parts of tool declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractTool<R,S> implements LogicalModelTool<R,S> {

	private final String formatID;
	private final String formatName;
	private final String helpMessage;
	private final boolean supportsMultivalued;
	
	protected AbstractTool(String id, String name) {
		this(id, name, "", false);
	}
	
	protected AbstractTool(String id, String name, String helpMessage, boolean supportsMultivalued) {
		this.formatID = id;
		this.formatName = name;
		this.helpMessage = helpMessage;
		this.supportsMultivalued = supportsMultivalued;
	}
	
	@Override
	public S getSettings() {
		return getSettings(new String[0]);
	}
	
	@Override
	public S getSettings(String s) {
		return getSettings(s.split(" "));
	}
	
	@Override
	public S getSettings(String ... parameters) {
		return null;
	}
	
	@Override
	public R getResult(LogicalModel model) throws Exception {
		return getResult(model, getSettings());
	}
	
	@Override
	public R getResult(LogicalModel model, String parameters) throws Exception {
		return getResult(model, getSettings(parameters));
	}
	
	@Override
	public R getResult(LogicalModel model, String ... parameters) throws Exception {
		return getResult(model, getSettings(parameters));
	}
	
	@Override
	public String getID() {
		return formatID;
	}

	@Override
	public String getName() {
		return formatName;
	}
	
	public String getHelp() {
		return helpMessage;
	}

	@Override
	public boolean supportsMultivalued() {
		return supportsMultivalued;
	}

	@Override
	public String toString() {
		return getID() +"\t"+ getName();
	}

}
