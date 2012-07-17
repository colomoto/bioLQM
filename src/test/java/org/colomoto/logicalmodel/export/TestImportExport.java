package org.colomoto.logicalmodel.export;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.stream.XMLStreamException;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.export.ginsim.LogicalModel2GINML;
import org.colomoto.logicalmodel.export.sbml.SBMLqualExport;
import org.colomoto.logicalmodel.export.sbml.SBMLqualImport;
import org.colomoto.logicalmodel.tool.inferinteraction.InteractionSearcher;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.junit.Test;

public class TestImportExport {

	@Test
	public void testGINMLExport() {

		LogicalModel model = getSimpleModel();
		exportGINML(model, "target/testExport.ginml");
	}

	@Test
	public void testSBML() {

		LogicalModel model = getSimpleModel();
		SBMLqualExport export = new SBMLqualExport(model);
		try {
			FileOutputStream out = new FileOutputStream("target/testExport.sbml");
			export.export(out);
		} catch (IOException e) {
			fail(e.getMessage());
		} catch (XMLStreamException e) {
			fail(e.getMessage());
		}
		

		try {
			File f = new File("target/testExport.sbml");
			SBMLqualImport simport = new SBMLqualImport(f);
			LogicalModel iModel = simport.getModel();
			
			exportGINML(iModel, "target/testExport_via_sbml.ginml");

		} catch (IOException e) {
			fail(e.getMessage());
		} catch (XMLStreamException e) {
			fail(e.getMessage());
		}

	}

	public void exportGINML(LogicalModel model, String path) {

		LogicalModel2GINML writer = new LogicalModel2GINML(model);
		try {
			FileOutputStream out = new FileOutputStream(path);
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

}
