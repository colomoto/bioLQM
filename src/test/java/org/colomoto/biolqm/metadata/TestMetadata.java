package org.colomoto.biolqm.metadata;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ReferenceModels;
import org.junit.jupiter.api.Test;

import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.colomoto.biolqm.NodeInfo;

public class TestMetadata {

	@Test
	public void testMetadataManagement() throws Exception {
		LogicalModel model = ReferenceModels.getModel("simpleFunctions.txt");

		// test modelMetadata
		Metadata modelMetadata = model.getMetadataOfModel();
		
		modelMetadata.addURI("lalaland", "unipro", "224587");
		Metadata uriMetadata = modelMetadata.getMetadataOfURI("lalaland", "unipro", "224587");
		
		uriMetadata.addAuthor("creator", "Martin", "Boutroux", "martinb@outlook.fr", "Inria", "1111444466668888");
		
		uriMetadata.addAuthor("creator", "Martin", "Boutroux", "martinb@outlook.fr", "Inria", "1111-4444-6666-8888");
		
		System.out.println(modelMetadata.getDescriptionMetadata());
		System.out.println(uriMetadata.getDescriptionMetadata());
		
		// test componentMetadata
		NodeInfo node = model.getComponents().get(0);
		
		Metadata nodeMetadata = model.getMetadataOfNode(node);
		
		nodeMetadata.addURI("isDescribedBy", "doi", "tintinaupaysdelornoir");
		nodeMetadata.addTag("isDescribedBy", "doi");
		nodeMetadata.addTag("isDescribedBy", "doi2");
		nodeMetadata.addTag("isDescribedBy", "doi");
		
		System.out.println(nodeMetadata.getDescriptionMetadata());
		
		nodeMetadata.removeTag("isDescribedBy", "doi");
		
		System.out.println(nodeMetadata.getDescriptionMetadata());
	}
}
