package org.colomoto.biolqm.metadata;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.LQMServiceManager;
import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.TestHelper;
import org.colomoto.biolqm.io.LogicalModelFormat;

import java.io.File;

import org.junit.jupiter.api.Test;

public class TestMetadata {

	@Test
	public void testMetadataManagement() throws Exception {
		
		File dir = TestHelper.getTestResource("reference_models");
		
		LogicalModelFormat format = LQMServiceManager.getFormat("sbml");
		
		if (!dir.isDirectory()) {
			throw new RuntimeException("Could not find the reference model folder: "+dir.getAbsolutePath());
		}
		
		if (format == null || !format.canLoad()) {
			throw new RuntimeException("Could not find the reference format");
		}
		
		LogicalModel model = format.load(new File(dir, "export_test_ginsim.sbml"));

		// test modelMetadata
		Metadata modelMetadata = model.getMetadataOfModel();
		
		modelMetadata.addURI("lalaland", "unipro", "224587");
		modelMetadata.addAuthor("creator", "Martin", "Boutroux", "martinb@outlook.fr", "Inria", "1111-4444-6666-8888");
		
		Metadata lalalandMetadata = modelMetadata.getMetadataOfQualifier("lalaland");
		
		lalalandMetadata.addAuthor("creator", "Martin", "Boutroux", "martinb@outlook.fr", "Inria", "1111-4444-6666-8888");
		
		System.out.println(modelMetadata.getDescriptionMetadata());
		System.out.println(lalalandMetadata.getDescriptionMetadata());
		
		// test componentMetadata
		NodeInfo node = model.getComponents().get(0);
		
		Metadata nodeMetadata = model.getMetadataOfNode(node);
		
		nodeMetadata.addURI("isDescribedBy", "doi", "tintinaupaysdelornoir");
		nodeMetadata.addTag("isDescribedBy", "doi");
		nodeMetadata.addTag("isDescribedBy", "doi2");
		nodeMetadata.addTag("isDescribedBy", "doi");
		
		for (NodeInfo element: model.getComponents()) {
			System.out.println(element.getNodeID());
			
			Metadata elementMetadata = model.getMetadataOfNode(element);
			System.out.println(elementMetadata.getDescriptionMetadata());
		}
		
		for (NodeInfo element: model.getExtraComponents()) {
			System.out.println(element.getNodeID());
			
			Metadata elementMetadata = model.getMetadataOfNode(element);
			System.out.println(elementMetadata.getDescriptionMetadata());
		}
		
		NodeInfo node2 = model.getComponents().get(1);
		
		Metadata elementMetadata = model.getMetadataOfNode(node2);
		Metadata nestedMetadata = elementMetadata.getMetadataOfQualifier("occursIn");
		System.out.println(nestedMetadata.getDescriptionMetadata());
	}
}
