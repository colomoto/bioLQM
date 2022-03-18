package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.TestHelper;
import org.colomoto.biolqm.io.LogicalModelFormat;

import java.io.File;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class TestMetadataInJson {
	
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
		
		// we add some metadata to the model
		AnnotationModule annot = model.getAnnotationModule();
		annot.selectModel();
		annot.selectQualifier("customQualifier");

		annot.addTag("word1");
		annot.addTag("word2");
		annot.addKeyValue("key1", "val11");
		annot.addKeyValue("key1", "value12");
		annot.addKeyValue("key2", "val21");

		annot.nested();
		annot.selectQualifier("is");

		annot.addCollectionEntry("uniprot", "P0DP23");
		annot.selectQualifier("is", 1);
		annot.addCollectionEntry("doi", "10.15252/msb.20199110");

		annot.nested();
		annot.selectQualifier("hasTag");
		annot.addTag("wordNested");
		annot.selectQualifier("hasKey");
		annot.addKeyValue("keyNested", "valueNested");

		annot.selectModel();

		// FIXME: legal terms
//		modelMetadata.addDateString("created", "2021-03-08");
//		modelMetadata.addDistribution("distributionTerms", "This document is under a free license.");
		
		// we add some metadata to a node
		for (NodeInfo node: model.getComponents()) {
			String nodeId = node.getNodeID();
			
			if (nodeId.equals("p53")) {
				annot.selectNode(node);

				// FIXME: legal terms
//				nodeMetadata.addAuthor("creator", "Martin", "Boutroux", null, null, null);
//				nodeMetadata.addAuthor("creator", "Dupond", "Dupont", "moulinsart@tintin.org", "Hergé", "0000-1111-2222-3333");
			}
		}
		
		// we load the same model and update its annotations with a json containing exactly the same annotations that we previously add
		if (!dir.isDirectory()) {
			throw new RuntimeException("Could not find the reference model folder: "+dir.getAbsolutePath());
		}
		if (format == null || !format.canLoad()) {
			throw new RuntimeException("Could not find the reference format");
		}

/*
		LogicalModel model2 = format.load(new File(dir, "minimal_example.sbml"));
		model2.importMetadata(dir.getAbsolutePath()+File.separator+"minimal_example_annotated.json");
		
		// and we compare the two of them to see if not problems were introduced
		Metadata model2Metadata = model2.getMetadataOfModel();
		
		boolean result = model2Metadata.sameMetadata(modelMetadata);
		
		assertEquals(result, true);
		
		for (NodeInfo node: model.getComponents()) {
			
			NodeInfo node2 = null;
        	for (NodeInfo element: model2.getComponents()) {
        		if(node.equals(element)) {
        			node2 = element;
        		}
        	}
			
			if (node2 != null) {
				Metadata nodeMeta = model.getMetadataOfNode(node);
				Metadata node2Meta = model2.getMetadataOfNode(node2);
				
				boolean resultNode = nodeMeta.sameMetadata(node2Meta);
				
				assertEquals(resultNode, true);
			} else {
				fail("The two models does not contain the same nodes.");
			}
		}
 */
	}
}
