package org.colomoto.biolqm.io;

import org.colomoto.TestHelper;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelComparator;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.junit.jupiter.api.Test;

import java.io.File;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Brute force tests for all formats supporting both import and export.
 * 
 * @author Aurelien Naldi
 */
public class TestBatchImports {
	
	@Test
	public void test() {

		File dir = TestHelper.getTestResource("import_models");
		if (!dir.isDirectory()) {
			fail("Could not find test resource: "+dir);
		}
		
		File[] groups = dir.listFiles();
		System.out.println("**********************************************");
		System.out.println("   Import checks: "+groups.length+" groups");
		System.out.println("**********************************************");
		for (File group: groups) {
			if (!group.isDirectory()) {
				continue;
			}
			
			// all files in this folder should be imported into identical models
			LogicalModel refModel = null;
			for (File f: group.listFiles()) {
				// guess format
				String name = f.getName();
				String extension = name.substring( name.lastIndexOf('.')+1 );
				LogicalModelFormat format = LQMServiceManager.getFormat(extension);
				if (format == null) {
					fail("Could not guess format for "+extension +" ("+f+")");
				}
				LogicalModel model;
				
				try {
					model = format.load(f);
					if (refModel == null) {
						refModel = model;
					} else {
						// check that the models are identical
						assertTrue( LogicalModelComparator.compare(refModel, model) );
					}
				} catch (Exception e) {
					fail();
				}
			}
		}

	}
}
