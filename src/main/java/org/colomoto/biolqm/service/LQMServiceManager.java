package org.colomoto.biolqm.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.tool.ModelToolService;

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
	private static final List<ModelToolService> tools = ExtensionLoader.load_instances(ModelToolService.class);
	private static final List<ModelModifierService> modifiers = ExtensionLoader.load_instances(ModelModifierService.class);

	private static final Map<String, LogicalModelFormat> id2format = new HashMap<String, LogicalModelFormat>();
	private static final Map<String, ModelToolService> id2tool = new HashMap<String, ModelToolService>();
	private static final Map<String, ModelModifierService> id2modifier = new HashMap<String, ModelModifierService>();

	private static Map<Class, Service> byClass = new HashMap<>();

	static {
		load(formats, id2format);
		load(tools, id2tool);
		load(modifiers, id2modifier);
	}

	private static <T extends Service> void load(List<T> services, Map<String, T> idmap) {
		for (T srv: services) {
			idmap.put( srv.getID(), srv);
			byClass.put(srv.getClass(), srv);
		}
		fillAliases(idmap, services);
	}

	private static <T extends Service> void fillAliases( Map<String, T> map, List<T> services) {
		for (T srv: services) {
			String[] aliases = srv.getAliases();
			if (aliases != null) {
				for (String alias: aliases) {
					if (map.containsKey(alias)) {
						continue;
					}
					map.put( alias, srv);
				}
			}

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
	public static ModelToolService getTool(String name) {

		return id2tool.get(name);
	}

	/**
	 * Get the available tools.
	 *
	 * @return all available tools
	 */
	public static Iterable<ModelToolService> getTools() {
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
	 * Retrieve any service by class
	 * 
	 * @param cl
	 * @return
	 */
	public static <T> T get(Class<T> cl) {
		return (T)byClass.get(cl);
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
		if (register(ModelToolService.class, srv, tools, id2tool)) {
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
