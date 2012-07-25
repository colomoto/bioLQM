package org.colomoto;

import java.io.File;

public class TestHelper {

	static File resourceFile;
	
	static {
		resourceFile = new File("target", "test-classes");
		if (!resourceFile.isDirectory() ) {
			throw new RuntimeException("No resource folder");
		}
	}
	
	public static File getTestResource(String name) {
		return getTestResource(null, name);
	}
	
	public static File getTestResource(String group, String name) {
		File dir = resourceFile;
		
		if (group != null && group.length() > 0) {
			dir = new File(resourceFile, group);
			if (!dir.isDirectory()) {
				throw new RuntimeException("resource group not found: "+dir.getAbsolutePath());
			}
		}
		
		return new File(dir, name);
	}
}
