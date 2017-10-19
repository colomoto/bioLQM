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
	 * Get the format descriptor instance for a given class.
	 *
	 * @param cl the class of the format
	 * @param <T> the type of the class and returned instance
	 * @return the format instance or null if not found.
	 */
	public static <T extends LogicalModelFormat> T getFormat(Class<T> cl) {
		return getByClass(getFormats(), cl);
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
	 * Get the tool service instance for a given class.
	 *
	 * @param cl the class of the tool service
	 * @param <T> the type of the class and returned instance
	 * @return the tool service instance or null if not found.
	 */
	public static <T extends LogicalModelTool> T getTool(Class<T> cl) {
		return getByClass(getTools(), cl);
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
	 * Get the service instance for a given class.
	 *
	 * @param cl the class of the service
	 * @param <T> the type of the class and returned instance
	 * @return the service instance or null if not found.
	 */
	public static <T extends ModelModifierService> T getModifier(Class<T> cl) {
		return getByClass(getModifiers(), cl);
	}

	/**
	 * Shared code to retrieve an element by class in a list of services.
	 * 
	 * @param l
	 * @param cl
	 * @return
	 */
	private static <S, T extends S> T getByClass(Iterable<S> l, Class<T> cl) {
		for (S srv: l) {
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
	
	/**
	 * Register a service after initial service discovery.
	 * 
	 * When possible, service discovery should be used instead.
	 * 
	 * @param srv the new service to register
	 * @return true is the service was added
	 */
	public static boolean register(Service srv) {
		if (srv == null) {
			return false;
		}
		
		if (register(ModelModifierService.class, srv, modifiers, id2modifier)) {
			return true;
		}
		if (register(LogicalModelTool.class, srv, tools, id2tool)) {
			return true;
		}
		if (register(LogicalModelFormat.class, srv, formats, id2format)) {
			return true;
		}
		
		return false;
	}
	
	private static <T> boolean register(Class<T> cl, Service srv, List<T> services, Map<String,T> id2srv) {
		String id = srv.getID();
		if (!cl.isInstance(srv) || id2srv.containsKey(id)) {
			return false;
		}
		
		services.add((T)srv);
		id2srv.put(id, (T)srv);
		return true;
	}
}
