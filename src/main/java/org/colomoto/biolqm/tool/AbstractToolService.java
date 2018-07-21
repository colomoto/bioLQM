package org.colomoto.biolqm.tool;

import org.colomoto.biolqm.LogicalModel;

/**
 * Base class for the boring parts of tool declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractToolService<R,S extends ToolSettings> implements ModelToolService<R,S> {

	private final String formatID;
	private final String[] aliases;
	private final String formatName;
	private final String helpMessage;
	private final boolean supportsMultivalued;
	
	protected AbstractToolService(String id, String name) {
		this(id, null, name, "", false);
	}

	protected AbstractToolService(String id, String name, String helpMessage, boolean supportsMultivalued) {
		this(id, null, name, helpMessage, supportsMultivalued);
	}

	protected AbstractToolService(String id, String[] aliases, String name, String helpMessage, boolean supportsMultivalued) {
		this.formatID = id;
		this.formatName = name;
		this.helpMessage = helpMessage;
		this.supportsMultivalued = supportsMultivalued;
		this.aliases = aliases;
	}
	
	@Override
	public S getSettings(LogicalModel model) {
		return getSettings(model, new String[0]);
	}
	
	@Override
	public S getSettings(LogicalModel model, String s) {
		return getSettings(model, s.split(" "));
	}
	
	@Override
	public R getResult(LogicalModel model) throws Exception {
		return getResult(getSettings(model));
	}
	
	@Override
	public R getResult(LogicalModel model, String parameters) throws Exception {
		return getResult(getSettings(model, parameters));
	}
	
	@Override
	public R getResult(LogicalModel model, String ... parameters) throws Exception {
		return getResult(getSettings(model, parameters));
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

	@Override
	public String[] getAliases() {
		return aliases;
	}
}
