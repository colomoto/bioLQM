package org.colomoto.biolqm.metadata;

import java.io.File;

import org.colomoto.TestHelper;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.LQMServiceManager;

import org.junit.jupiter.api.Test;

public class TestAddOfURIs {
	
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
		Annotator annot = model.getAnnotator();

		annot.onModel().openBlock("is");
		annot.annotate("uniprot:P0DP23");
		annot.annotate("urn:miriam:pubmed:31226023");
		annot.annotate("urn:casq:function:Orf9b");
		annot.annotate("https://www.uniprot.org/uniprot/P0DP23");
		annot.annotate("https://identifiers.org/doi:10.1093/ajae/aaq063");


		// FIXME: load and compare metadata content

//		LogicalModel model2 = format.load(new File(dir, "minimal_example.sbml"));
//		model2.importMetadata(dir.getAbsolutePath()+File.separator+"uris_examples.json");

		// and we compare the two of them to see if not problems were introduced
//		Metadata model2Metadata = model2.getMetadataOfModel();
//		assertTrue(model2Metadata.sameMetadata(modelMetadata));
	}
}
