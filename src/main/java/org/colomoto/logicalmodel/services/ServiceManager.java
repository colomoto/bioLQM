package org.colomoto.logicalmodel.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

import org.colomoto.logicalmodel.io.LogicalModelFormat;

/**
 * List available "services".
 * For now it only manages formats, to be further extended...
 * 
 * @author Aurelien Naldi
 */
public class ServiceManager {

	private static ServiceManager MANAGER = null;
	
	/**
	 * Retrieve the single-instance service manager.
	 * 
	 * @return the service manager
	 */
	public static ServiceManager getManager() {
		if (MANAGER == null) {
			MANAGER = new ServiceManager();
		}
		return MANAGER;
	}
	
	private final List<LogicalModelFormat> formats;
	private final Map<String, LogicalModelFormat> id2format = new HashMap<String, LogicalModelFormat>();
	
	private ServiceManager() {
		formats = new ArrayList<LogicalModelFormat>();
		
        Iterator<LogicalModelFormat> service_list = ServiceLoader.load( LogicalModelFormat.class).iterator(); 
        while (service_list.hasNext()) {
            try {
            	LogicalModelFormat format = service_list.next();
            	if( format != null){
            		formats.add(format);
        			id2format.put( format.getID(), format);
            	}
            }
            catch (ServiceConfigurationError e){

            }
        }
		
		for (LogicalModelFormat format: formats) {
			id2format.put( format.getID(), format);
		}
	}
	
	/**
	 * Get the format declaration for a given ID.
	 * 
	 * @param name ID of the format
	 * @return the format declaration instance or null if not found.
	 */
	public LogicalModelFormat getFormat(String name) {
		
		return id2format.get(name);
	}
	
	/**
	 * Get the available formats.
	 * 
	 * @return all available formats
	 */
	public Iterable<LogicalModelFormat> getFormats() {
		return formats;
	}
}
