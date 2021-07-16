package org.colomoto.biolqm.metadata;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;

import org.colomoto.TestHelper;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.metadata.annotations.Metadata;
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
		LogicalModel model2 = format.load(new File(dir, "minimal_example.sbml"));
		
		// we get the nodes of the first model
		NodeInfo nodep53 = null;
		NodeInfo nodeMdm2cyt = null;
		NodeInfo nodeDNAdam = null;
		
		for (NodeInfo node: model.getComponents()) {
			String nodeId = node.getNodeID();
			
			if (nodeId.equals("p53")) {
				nodep53 = node;
			} else if (nodeId.equals("Mdm2cyt")) {
				nodeMdm2cyt = node;
			} else if (nodeId.equals("DNAdam")) {
				nodeDNAdam = node;
			}
		}
		
		if (nodep53 == null) {
			fail("The model does not contain the node p53.");
		} else if (nodeMdm2cyt == null) {
			fail("The model does not contain the node Mdm2cyt.");
		} else if (nodeDNAdam == null) {
			fail("The model does not contain the node DNAdam.");
		}
		
		// we add some metadata to 2 pairs of nodes
		Metadata meta1 = model.getMetadataOfEdge(nodep53, nodeMdm2cyt);
		Metadata meta2 = model.getMetadataOfEdge(nodeDNAdam, nodeMdm2cyt);
		
		meta1.addTag("customQualifier", "word1");
		meta1.addTag("customQualifier", "word2");
		meta1.addKeyValue("customQualifier", "key1", "val11");
		meta1.addKeyValue("customQualifier", "key1", "value12");
		meta1.addKeyValue("customQualifier", "key2", "val21");
		
		meta2.addURI("is", "uniprot:P0DP23");
		meta2.createAlternative("is");
		meta2.addURI("is", 1, "doi:10.15252/msb.20199110");
		
		model.exportMetadata(dir.getAbsolutePath()+File.separator+"minimal_example_edges_annotated.json");
		
		model2.importMetadata(dir.getAbsolutePath()+File.separator+"minimal_example_edges_annotated.json");
		
		// we get the nodes of the second model
		NodeInfo node2p53 = null;
		NodeInfo node2Mdm2cyt = null;
		NodeInfo node2DNAdam = null;
		
		for (NodeInfo node: model2.getComponents()) {
			String nodeId = node.getNodeID();
			
			if (nodeId.equals("p53")) {
				node2p53 = node;
			} else if (nodeId.equals("Mdm2cyt")) {
				node2Mdm2cyt = node;
			} else if (nodeId.equals("DNAdam")) {
				node2DNAdam = node;
			}
		}
		
		Metadata meta3 = model2.getMetadataOfEdge(node2p53, node2Mdm2cyt);
		Metadata meta4 = model2.getMetadataOfEdge(node2DNAdam, node2Mdm2cyt);
		
		boolean result1 = meta1.sameMetadata(meta3);
		assertEquals(result1, true);
		
		boolean result2 = meta2.sameMetadata(meta4);
		assertEquals(result2, false);
	}
}
