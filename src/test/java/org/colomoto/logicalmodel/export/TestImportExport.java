package org.colomoto.logicalmodel.export;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.stream.XMLStreamException;

import junit.framework.TestCase;

import org.colomoto.TestHelper;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.colomoto.logicalmodel.io.ginml.LogicalModel2GINML;
import org.colomoto.logicalmodel.io.rawfunctions.TextFunctionFormat;
import org.colomoto.logicalmodel.io.sbml.SBMLqualExport;
import org.colomoto.logicalmodel.io.sbml.SBMLqualImport;
import org.colomoto.logicalmodel.services.ServiceManager;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.Test;

public class TestImportExport extends TestCase {

	@Test
	public void testGINMLExport() {

		LogicalModel model = getSimpleModel();
		exportGINML(model, "testExport.ginml");
	}

	@Test
	public void testSBMLExport() {

		LogicalModel model = getSimpleModel();
		
		exportSBML(model, "testExport.sbml");

		importSBML("testExport.sbml");
	}

	private void importSBML(String filename) {
		try {
			File f = TestHelper.getTestOutput(filename);
			SBMLqualImport simport = new SBMLqualImport(f);
			LogicalModel iModel = simport.getModel();
			exportGINML(iModel, filename+".ginml");

		} catch (IOException e) {
			fail(e.getMessage());
		} catch (XMLStreamException e) {
			fail(e.getMessage());
		}
	}

	public void exportSBML(LogicalModel model, String filename) {
		SBMLqualExport export = new SBMLqualExport(model);
		try {
			FileOutputStream out = new FileOutputStream(TestHelper.getTestOutput(filename));
			export.export(out);
		} catch (IOException e) {
			fail(e.getMessage());
		} catch (XMLStreamException e) {
			fail(e.getMessage());
		}

	}

	
	public void exportGINML(LogicalModel model, String filename) {

		LogicalModel2GINML writer = new LogicalModel2GINML(model);
		try {
			FileOutputStream out = new FileOutputStream(TestHelper.getTestOutput(filename));
			writer.export(out);
		} catch (IOException e) {
			fail(e.getMessage());
		}
		
	}

	public static LogicalModel getSimpleModel() {
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		byte max = 1;
		variables.add(new NodeInfo("v1", max));
		variables.add(new NodeInfo("v2", max));
		variables.add(new NodeInfo("v3", max));
		
		
		MDDManager ddmanager = MDDManagerFactory.getManager(variables, 5);
		MDDVariable[] ddVariables = ddmanager.getAllVariables();
		int[] functions = new int[3];
		
		int v1 = ddVariables[0].getNode(0, 1);
		int v2 = ddVariables[1].getNode(0, 1);
		int nv2 = ddVariables[1].getNode(1, 0);
		int v1nv2 = MDDBaseOperators.AND.combine(ddmanager, v1, nv2);
		int v1v2 = MDDBaseOperators.AND.combine(ddmanager, v1, v2);
		
		functions[0] = v1;
		functions[1] = v1nv2;
		functions[2] = v1v2;
		
		return new LogicalModelImpl(ddmanager, variables, functions, new ArrayList<NodeInfo>(), new int[0]);
	}

	@Test
	public void testRawFunctionImport() throws IOException {
		File f = TestHelper.getTestResource("simpleFunctions.txt");
		LogicalModelFormat format = ServiceManager.getManager().getFormat(TextFunctionFormat.ID);
		LogicalModel model = format.importFile(f);

		List<NodeInfo> coreComponents = model.getNodeOrder();
		
		assertEquals(5, coreComponents.size());
		
		// TODO: test model content
	}
	
	
	@Test
	public void testRawFunctionExport() throws IOException {
		File f = TestHelper.getTestOutput("exportFunctions.txt");
		LogicalModelFormat format = ServiceManager.getManager().getFormat(TextFunctionFormat.ID);
		format.export(getSimpleModel(), new FileOutputStream(f));
	}
}
