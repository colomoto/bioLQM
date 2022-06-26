package org.colomoto.biolqm.metadata;

import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;

import org.colomoto.TestHelper;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.junit.jupiter.api.Test;

public class TestMetadataOnEdge {
	
	@Test
	public void testMetadataManagement() throws Exception {
		
		// we retrieve the minimal_example sbml file
		File dir = TestHelper.getTestResource("sbml_models");
		LogicalModelFormat format = LQMServiceManager.getFormat("sbml");
		
		if (!dir.isDirectory()) {
			throw new RuntimeException("Could not find the reference model folder: "+dir.getAbsolutePath());
		}
		if (format == null || !format.canLoad()) {
			throw new RuntimeException("Could not find the reference format");
		}
		
		LogicalModel model = format.load(new File(dir, "minimal_example.sbml"));

		// we get the nodes of the first model
		NodeInfo nodep53 = null;
		NodeInfo nodeMdm2cyt = null;
		NodeInfo nodeDNAdam = null;
		
		for (NodeInfo node: model.getComponents()) {
			switch (node.getNodeID()) {
				case "p53":
					nodep53 = node;
					break;
				case "Mdm2cyt":
					nodeMdm2cyt = node;
					break;
				case "DNAdam":
					nodeDNAdam = node;
					break;
			}
		}
		
		if (nodep53 == null || nodeMdm2cyt == null || nodeDNAdam == null) {
			fail("The model does not contain the nodes p53, Mdmcyt, and DNAdam.");
		}

		Annotator<NodeInfo> annot = model.getAnnotator();

		annot.edge(nodep53, nodeMdm2cyt)
			.openBlock("customQualifier")
			.tag("word1")
			.tag("word2")
			.put("key1", "val11")
			.put("key1", "value12")
			.put("key2", "val21");

		annot.edge(nodeDNAdam, nodeMdm2cyt)
			.openBlock("is")
			.identifier("uniprot", "P0DP23")
			.openBlock("is")
			.identifier("doi", "10.15252/msb.20199110");


		String output = dir.getAbsolutePath()+File.separator+"minimal_example_edges_annotated.json";
		model.saveAnnotation(output);

		// FIXME: compare saved and loaded annotations
		LogicalModel model2 = format.load(new File(dir, "minimal_example.sbml"));
//		model2.importMetadata(dir.getAbsolutePath()+File.separator+"minimal_example_edges_annotated.json");
	}
}
