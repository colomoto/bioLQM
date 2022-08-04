package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.TestHelper;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class TestMetadataInJson {
	
	@Test
	public void testMetadataManagement() throws Exception {
		
		// we retrieve the minimal_example sbml file
		String inputname = TestHelper.getTestFilename("sbml_models", "minimal_example.sbml");
		String outputname = TestHelper.getTestFilename("sbml_models", "minimal_example_annotation.json");
		String outputname2 = TestHelper.getTestFilename("sbml_models", "minimal_example_annotation2.json");
		String jsonname = TestHelper.getTestFilename("sbml_models", "minimal_example_annotated.json");

		LogicalModel model = LQMServiceManager.load(inputname);
		
		// we add some metadata to the model
		Annotator<NodeInfo> annot = model.getAnnotator();

		annot.onModel().openBlock("customQualifier");
		annot.annotate("#word1");
		annot.annotate("#word2");
		annot.annotate("key1 = val11");
		annot.annotate("key1=value12");
		annot.annotate("key2=val21");

//		annot.nested()
//			.qualify("is")
//			.identifier("uniprot", "P0DP23")
//			.qualify("is", 1)
//			.identifier("doi", "10.15252/msb.20199110");

//		annot.nested()
//			.qualify("hasTag")
//			.tag("wordNested")
//			.qualify("hasKey")
//			.put("keyNested", "valueNested");

		annot.onModel();

		// FIXME: legal terms
//		modelMetadata.addDateString("created", "2021-03-08");
//		modelMetadata.addDistribution("distributionTerms", "This document is under a free license.");
		
		// we add some metadata to a node
		NodeInfo ni = model.getComponent("p53");
		if (ni != null) {
			annot.node(ni).annotate("#output");
		}

		NodeInfo ni_mdm2 = model.getComponent("Mdm2cyt");
		if (ni != null && ni_mdm2 != null) {
			annot.edge(ni, ni_mdm2).annotate("#test");
		}
		
		// we load the same model and update its annotations with a json containing exactly the same annotations that we previously add
		model.saveAnnotation(outputname);

		LogicalModel model2 = LQMServiceManager.load(inputname);
		Annotator<NodeInfo> annot2 = model2.getAnnotator();
		annot2.importMetadata(outputname, model2.getComponents());

		model2.saveAnnotation(outputname2);

		// TODO: compare old and parsed metadata
	}
}
