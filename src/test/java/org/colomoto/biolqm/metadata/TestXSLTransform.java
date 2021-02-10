package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.metadata.annotations.XSLTransform;

import org.junit.jupiter.api.Test;

public class TestXSLTransform {
	
	@Test
	public void testMetadataManagement() throws Exception {
		
		//XSLTransform.setPropertiesXSLTransform();
		
		System.out.println(XSLTransform.simpleTransform("<html><body><h2>An Unordered HTML List</h2><ul><li>Coffee</li><li>Tea</li><li>Milk</li></ul>  <h2>An Ordered HTML List</h2><ol><li>Coffee</li><li>Tea</li><li>Milk</li></ol> </body></html>"));
	}
}