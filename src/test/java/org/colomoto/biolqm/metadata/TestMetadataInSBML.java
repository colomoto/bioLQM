package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.metadata.validations.PatternValidator;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.TestHelper;
import org.colomoto.biolqm.io.LogicalModelFormat;

import java.io.File;
import java.util.regex.Matcher;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

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

		Annotator<NodeInfo> annot = model.getAnnotator();
		
		// we add some metadata to the model
//		LegalAnnotation legal = annot.getLegal();
//		legal.setCreated("2021-03-08");

		annot.onModel()
			.qualify("customQualifier")
			.tag("word1")
			.tag("word2")
			.put("key1", "val11")
			.put("key1", "value12")
			.put("key2", "val21")
			.nested()
			.qualify("is")
			.identifier("uniprot", "P0DP23");

		annot.qualify("is", 1)
			.identifier("doi", "10.15252/msb.20199110");

		annot.nested()
			.qualify("hasTag")
			.tag("wordNested")
			.qualify("hasKey")
			.put("keyNested", "valueNested");
		
		// we add some metadata to a node
		for (NodeInfo node: model.getComponents()) {
			String nodeId = node.getNodeID();
			
			if (nodeId.equals("p53")) {
				annot.node(node);
//				nodeMetadata.addAuthor("creator", "Martin", "Boutroux", null, null, null);
//				nodeMetadata.addAuthor("creator", "Dupond", "Dupont", "moulinsart@tintin.org", "Hergé", null);
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
		
		LogicalModel model2 = format.load(new File(dir, "minimal_example_saved.sbml"));
		Annotator<NodeInfo> annot2 = model2.getAnnotator();
		LQMServiceManager.save(model2, dir.getAbsolutePath()+File.separator+"minimal_example_saved_again.sbml", "sbml");

		// FIXME: compare annotations of the two models
	}

	@Test
	public void testMatching() {
		Matcher m = PatternValidator.matchTag("#pipo");
		assertTrue(m.matches());
		assertEquals("pipo", m.group(1));

		m = PatternValidator.matchTag("tag:pipo");
		assertTrue(m.matches());
		assertEquals("pipo", m.group(1));

		assertFalse(PatternValidator.matchTag("tg:pipo").matches());
		assertFalse(PatternValidator.matchTag("col:pipo").matches());
		assertFalse(PatternValidator.matchTag("pipo").matches());

	}
}
