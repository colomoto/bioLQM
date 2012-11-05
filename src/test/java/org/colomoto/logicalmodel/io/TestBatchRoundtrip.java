package org.colomoto.logicalmodel.io;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestResult;

import org.colomoto.TestHelper;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.ReferenceModels;
import org.colomoto.logicalmodel.services.ServiceManager;
import org.junit.Test;
import org.sbml.jsbml.xml.test.GetNotesStringTests;

/**
 * Brute force tests for all formats supporting both import and export.
 * 
 * @author Aurelien Naldi
 */
public class TestBatchRoundtrip {
	
	@Test
	public void test() {

		String[] names = ReferenceModels.getNames();
		List<LogicalModelFormat> ioformats = new ArrayList<LogicalModelFormat>();
		for (LogicalModelFormat format: ServiceManager.getManager().getFormats()) {
			if (format.canImport() && format.canExport()) {
				ioformats.add(format);
			}
		}
		System.out.println("*************************************************");
		System.out.println("     E/I roundtrips: "+names.length+" models ; "+ioformats.size()+" formats");
		System.out.println("*************************************************");
		
		boolean failedModels = false;
		for (String name: names) {
			LogicalModel model = null;
			try {
				model = ReferenceModels.getModel(name);
			} catch (Exception e) {}
			
			if (model == null) {
				System.out.println("[FAIL] "+name);
				failedModels = true;
				continue;
			}
			System.out.println("* "+name);
			
			for (LogicalModelFormat format: ioformats) {
				System.out.println("   - "+format.getID());
				try {
					roundtrip(format, name, model);
				} catch (Exception e) {
					fail("Format "+format.getID()+" failed on "+name+"\nMessage: "+e.getMessage());
				}
			}
		}
		
		if (failedModels) {
			fail("Some models could not be loaded");
		}
	}

	/**
	 * Test a format on a specific model.
	 * This will export the model, then import it back and check that the result is consistent.
	 * 
	 * @param format
	 * @param model
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	private void roundtrip(LogicalModelFormat format, String name, LogicalModel model) throws FileNotFoundException, IOException {
		String ioName = name+"."+format.getID();
		File f = TestHelper.getTestOutput("io_roundtrips", ioName);
		format.export(model, new FileOutputStream(f));
		LogicalModel importedModel = format.importFile(f);
		// TODO: check if both models are identical
	}
}
