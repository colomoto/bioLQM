package org.colomoto.logicalmodel.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.colomoto.logicalmodel.io.ginml.GINMLFormat;
import org.colomoto.logicalmodel.io.sbml.SBMLFormat;

public class ServiceManager {

	private static ServiceManager MANAGER = null;
	
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
		
		// hardcoded formats for now, use mangosdk.spi.ProviderFor in the future ?
		formats.add(new GINMLFormat());
		formats.add(new SBMLFormat());
		
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
	 * @return
	 */
	public Iterable<LogicalModelFormat> getFormats() {
		return formats;
	}
}
