package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.metadata.validations.PatternValidator;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.TestHelper;

import java.util.regex.Matcher;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class TestMetadataInSBML {
	
	@Test
	public void testMetadataManagement() {

		// we retrieve the minimal_example sbml file
		String loadname  = TestHelper.getTestFilename("sbml_models", "minimal_example.sbml");
		String savename  = TestHelper.getTestFilename("sbml_models", "minimal_example_saved.sbml");
		String savename2 = TestHelper.getTestFilename("sbml_models", "minimal_example_saved_again.sbml");
		LogicalModel model = LQMServiceManager.load(loadname);
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
			.put("key2", "val21");
//		annot.nested()
//			.qualify("is")
//			.identifier("uniprot", "P0DP23");

		annot.onModel()
			.qualify("is", 1)
			.identifier("doi", "10.15252/msb.20199110");

//		annot.nested()
//			.qualify("hasTag")
//			.tag("wordNested")
//			.qualify("hasKey")
//			.put("keyNested", "valueNested");
		
		// we add some metadata to a node
		NodeInfo ni = model.getComponent("p53");
		if (ni != null) {
			annot.node(ni).tag("output");
		}

		// we save the model
		LQMServiceManager.save(model, savename, "sbml");
		
		// we load a sbml model with exactly the same annotations
		LogicalModel model2 = LQMServiceManager.load(savename);
		Annotator<NodeInfo> annot2 = model2.getAnnotator();
		LQMServiceManager.save(model, savename2, "sbml");

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
