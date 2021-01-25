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
		
		File dir = TestHelper.getTestResource("sbml_models");
		
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
		modelMetadata.addAuthor("creator", "Martin", "", null, null, "1111-4444-6666-8888");
		
		Metadata lalalandMetadata = modelMetadata.getMetadataOfQualifier("lalaland");
		
		lalalandMetadata.addAuthor("creator", "Martin", null, null, "Inria", "1111-4444-6666-8888");
		
		// System.out.println(modelMetadata.getDescriptionMetadata());
		// System.out.println(lalalandMetadata.getDescriptionMetadata());
		
		// test componentMetadata
		NodeInfo node = model.getComponents().get(0);
		
		Metadata nodeMetadata = model.getMetadataOfNode(node);
		
		modelMetadata.addURI("isDescribedBy", "doi", "tintinaupaysdelornoir");
		nodeMetadata.addTag("isDescribedBy", "doi");
		nodeMetadata.addTag("isDescribedBy", "doi2");
		nodeMetadata.addTag("isDescribedBy", "doi");
		
		nodeMetadata.addAuthor("creator", "Aurelien", "Naldi", "aureliennaldi@inria.fr", "Inria", "1111-4444-6666-8888");
		
		nodeMetadata.createAlternative("occursIn");
		nodeMetadata.createAlternative("is");
		nodeMetadata.addURI("is", 1, "doi", "fantsaio");
		
		modelMetadata.addURI("isDescribedBy", "doi", "10.1002/0470841559.ch1");
		
		Metadata nodenestedMetadata = nodeMetadata.getMetadataOfQualifier("is", 1);
		
		nodenestedMetadata.addURI("isDescribedBy", "doi", "maissicesttintinaupaysdelornoir");
		nodenestedMetadata.addTag("isDescribedBy", "doi");
		nodenestedMetadata.addTag("isDescribedBy", "doi2");
		nodenestedMetadata.addTag("isDescribedBy", "doi");
		
		nodenestedMetadata.createAlternative("isDescribedBy");
		nodenestedMetadata.addURI("isDescribedBy", 1, "doi", "jesaispassicesttintinaupaysdelornoir");
		
		Metadata nodenestedMetadata2 = nodenestedMetadata.getMetadataOfQualifier("isDescribedBy", 1);
		nodenestedMetadata2.addURI("is", "doi", "ahbahnoncestspirou");
		
		nodenestedMetadata2.addTag("isDescribedBy", "doi");
		
		// for (NodeInfo element: model.getComponents()) {
			// System.out.println(element.getNodeID());
			
			// Metadata elementMetadata = model.getMetadataOfNode(element);
			// System.out.println(elementMetadata.getDescriptionMetadata());
		// }

		// NodeInfo node2 = model.getComponents().get(1);
		
		// Metadata elementMetadata = model.getMetadataOfNode(node2);
		// Metadata nestedMetadata = elementMetadata.getMetadataOfQualifier("occursIn");
		// System.out.println(nestedMetadata.getDescriptionMetadata());
		
		// System.out.println(modelMetadata.getNotes());
		
		NodeInfo node2 = model.getComponents().get(2);
		
		Metadata elementMetadata = model.getMetadataOfNode(node2);
		
		elementMetadata.addTag("isDescribedBy", "doi");
		elementMetadata.addTag("isDescribedBy", "doi2");
		
		nodenestedMetadata.removeTag("isDescribedBy", "doi");
		nodenestedMetadata.removeURI("isDescribedBy", 1, "doi", "jesintinaupaysdelornoir");
		nodenestedMetadata.removeURI("isDescribedBy", 1, "doi", "jesaispassicesttintinaupaysdelornoir");
		
		elementMetadata.addAuthor("creator", "Martin", "Boutroux", "martinb@outlook.fr", "Inria", "1111-4444-6666-8888");
		
		elementMetadata.addURI("isDescribedBy", "doi", "10.1002/0470841559.ch1");
		elementMetadata.addURI("isDescribedBy", "doi", "10.1093/ajae/aaq063");
		
		LQMServiceManager.save(model, dir.getAbsolutePath()+"\\export_test_ginsim_output.sbml", "sbml");
		
		System.out.println(elementMetadata.getHelpAnnotation("isDescribedBy"));
		System.out.println(elementMetadata.getHelpMetadata());
	}
}
