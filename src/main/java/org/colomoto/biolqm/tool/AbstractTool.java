package org.colomoto.biolqm.tool;

import org.colomoto.biolqm.LogicalModel;

/**
 * Base class for the boring parts of tool declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractTool implements LogicalModelTool {

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
	public void run(LogicalModel model) {
		run(model, "");
	}
	
	@Override
	public void run(LogicalModel model, String parameters) {
		String[] parameterArray = parameters.split(" ");
		run(model, parameterArray);
	}
}
