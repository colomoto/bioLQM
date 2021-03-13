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

public class TestMetadataInSBML {
	
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
		Metadata modelMetadata = model.getAnnotationModule().getMetadataOfModel();
		
		modelMetadata.addTag("customQualifier", "word1");
		modelMetadata.addTag("customQualifier", "word2");
		modelMetadata.addKeyValue("customQualifier", "key1", "val11");
		modelMetadata.addKeyValue("customQualifier", "key1", "value12");
		modelMetadata.addKeyValue("customQualifier", "key2", "val21");
		
		Metadata nestedMetadata = modelMetadata.getMetadataOfQualifier("customQualifier");
		
		nestedMetadata.addURI("is", "uniprot", "P0DP23");
		nestedMetadata.createAlternative("is");
		nestedMetadata.addURI("is", 1, "doi", "10.15252/msb.20199110");
		
		Metadata doubleNestedMetadata = nestedMetadata.getMetadataOfQualifier("is");
		
		doubleNestedMetadata.addTag("hasTag", "wordNested");
		doubleNestedMetadata.addKeyValue("hasKey", "keyNested", "valueNested");
		
		modelMetadata.addDate("created", "2021-03-08");
		
		// we add some metadata to a node
		for (NodeInfo node: model.getComponents()) {
			String nodeId = node.getNodeID();
			
			if (nodeId.equals("p53")) {
				Metadata nodeMetadata = model.getAnnotationModule().getMetadataOfNode(node);
				
				nodeMetadata.addAuthor("creator", "Martin", "Boutroux", null, null, null);
				nodeMetadata.addAuthor("creator", "Dupond", "Dupont", "moulinsart@tintin.org", "Hergé", null);
			}
		}
		
		// we save the model
		LQMServiceManager.save(model, dir.getAbsolutePath()+File.separator+"minimal_example_saved.sbml", "sbml");
		
		// we load a sbml model with exactly the same annotations
		if (!dir.isDirectory()) {
			throw new RuntimeException("Could not find the reference model folder: "+dir.getAbsolutePath());
		}
		if (format == null || !format.canLoad()) {
			throw new RuntimeException("Could not find the reference format");
		}
		
		LogicalModel model2 = format.load(new File(dir, "minimal_example_annotated.sbml"));

		// and we compare the two of them to see if not problems were introduced
		Metadata model2Metadata = model2.getAnnotationModule().getMetadataOfModel();
		
		boolean result = model2Metadata.sameMetadata(modelMetadata);
		assertEquals(result, true);
		
		for (NodeInfo node: model.getComponents()) {
			Metadata nodeMeta = model.getAnnotationModule().getMetadataOfNode(node);
			Metadata node2Meta = model2.getAnnotationModule().getMetadataOfNode(node);
			
			if (node2Meta != null) {
				boolean resultNode = nodeMeta.sameMetadata(node2Meta);
				assertEquals(resultNode, true);
			} else {
				fail("The two models does not contain the same nodes.");
			}
		}
	}
}
