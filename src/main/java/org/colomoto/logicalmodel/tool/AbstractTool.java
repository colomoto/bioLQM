package org.colomoto.logicalmodel.tool;

/**
 * Base class for the boring parts of tool declaration classes.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractTool implements LogicalModelTool {

	private final String formatID;
	private final String formatName;
	private final boolean supportsMultivalued;
	
	protected AbstractTool(String id, String name) {
		this(id, name, false);
	}
	
	protected AbstractTool(String id, String name, boolean supportsMultivalued) {
		this.formatID = id;
		this.formatName = name;
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

	@Override
	public boolean supportsMultivalued() {
		return supportsMultivalued;
	}

	@Override
	public String toString() {
		return getID() +"\t"+ getName();
	}
}
