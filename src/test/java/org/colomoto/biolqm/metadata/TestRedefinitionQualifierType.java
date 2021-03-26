package org.colomoto.biolqm.metadata;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;

import org.colomoto.TestHelper;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.junit.jupiter.api.Test;

public class TestRedefinitionQualifierType {
	
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
		Metadata modelMetadata = model.getMetadataOfModel();
		modelMetadata.addTag("is", "word1");
		Metadata nestedMetadata = modelMetadata.getMetadataOfQualifier("is");
		
		// customQualifier (in model components) does not have any instances yet so its type is null
		assertEquals(nestedMetadata.suitedJavaClass("customQualifier"), null);
		
		// we initialize the type of customQualifier as a GenericAnnotation (in model components) 
		nestedMetadata.addURI("customQualifier", "uniprot", "P0DP23");
		assertEquals(nestedMetadata.suitedJavaClass("customQualifier"), "GenericAnnotation");
		
		nestedMetadata.removeURI("customQualifier", "uniprot", "P0DP23");

		// customQualifier (in model components) does not have any instances anymore so its type is null
		assertEquals(nestedMetadata.suitedJavaClass("customQualifier"), null);
		
		// we reinitialize the type of customQualifier (in model components) as a DistributionAnnotation
		modelMetadata.addDistribution("customQualifier", "coucou");
		assertEquals(nestedMetadata.suitedJavaClass("customQualifier"), "DistributionAnnotation");
	}
}