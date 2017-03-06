package org.colomoto.biolqm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.tool.LogicalModelTool;

/**
 * Static service manager: list available "services" in LQM:
 * <ul>
 *   <li>formats</li>
 *   <li>modifiers</li>
 * </ul>
 * @author Aurelien Naldi
 */
public class LQMServiceManager {
	
	private static final List<LogicalModelFormat> formats = ExtensionLoader.load_instances(LogicalModelFormat.class);
	private static final List<LogicalModelTool> tools = ExtensionLoader.load_instances(LogicalModelTool.class);
	private static final List<ModelModifierService> modifiers = ExtensionLoader.load_instances(ModelModifierService.class);

	private static final Map<String, LogicalModelFormat> id2format = new HashMap<String, LogicalModelFormat>();
	private static final Map<String, LogicalModelTool> id2tool = new HashMap<String, LogicalModelTool>();
	private static final Map<String, ModelModifierService> id2modifier = new HashMap<String, ModelModifierService>();

	static {
		for (LogicalModelFormat format: formats) {
			id2format.put( format.getID(), format);
        }
		for (LogicalModelTool tool: tools) {
			id2tool.put( tool.getID(), tool);
        }
		for (ModelModifierService modifier: modifiers) {
			id2modifier.put( modifier.getID(), modifier);
        }
	}
	
	/**
	 * Get the format declaration for a given ID.
	 * 
	 * @param name ID of the format
	 * @return the format declaration instance or null if not found.
	 */
	public static LogicalModelFormat getFormat(String name) {
		return id2format.get(name);
	}
	
	/**
	 * Get the available formats.
	 * 
	 * @return all available formats
	 */
	public static Iterable<LogicalModelFormat> getFormats() {
		return formats;
	}

	/**
	 * Get the tool declaration for a given ID.
	 *
	 * @param name ID of the tool
	 * @return the tool declaration instance or null if not found.
	 */
	public static LogicalModelTool getTool(String name) {

		return id2tool.get(name);
	}

	/**
	 * Get the available tools.
	 *
	 * @return all available tools
	 */
	public static Iterable<LogicalModelTool> getTools() {
		return tools;
	}

	/**
	 * Get the service for a given ID.
	 *
	 * @param name ID of the service
	 * @return the service instance or null if not found.
	 */
	public static ModelModifierService getModifier(String name) {
		return id2modifier.get(name);
	}

	/**
	 * Get the service instance for a given class
	 *
	 * @param cl the class of the service
	 * @param <T> the type of the class and returned instance
	 * @return the service instance or null if not found.
	 */
	public static <T extends ModelModifierService> T getModifier(Class<T> cl) {
		for (ModelModifierService srv: getModifiers()) {
			if (cl.isInstance(srv)) {
				return (T)srv;
			}
		}
		return null;
	}

	/**
	 * Get the available model modifier services.
	 *
	 * @return all available services for model modifiers
	 */
	public static Iterable<ModelModifierService> getModifiers() {
		return modifiers;
	}
}
