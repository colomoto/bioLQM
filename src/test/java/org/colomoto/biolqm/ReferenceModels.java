package org.colomoto.biolqm;

import java.io.File;
import java.io.IOException;

import org.colomoto.TestHelper;
import org.colomoto.biolqm.io.StreamProviderFileImpl;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.LQMServiceManager;

public class ReferenceModels {

	private static final File dir = TestHelper.getTestResource("reference_models");
	private static LogicalModelFormat format = LQMServiceManager.getFormat("boolfunctions");
	
	
	static {
		if (!dir.isDirectory()) {
			throw new RuntimeException("Could not find the reference model folder: "+dir.getAbsolutePath());
		}
		
		if (format == null || !format.canLoad()) {
			throw new RuntimeException("Could not find the reference format");
		}
	}
	
	public static String[] getNames() {
		return dir.list();
	}
	
	public static LogicalModel getModel(String name) throws Exception {
		return format.load(new StreamProviderFileImpl( new File(dir, name)));
	}

	/**
	 * Private constructor: pure static class
	 */
	private ReferenceModels() {
	}
}
