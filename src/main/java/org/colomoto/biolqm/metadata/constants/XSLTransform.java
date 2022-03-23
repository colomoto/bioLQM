package org.colomoto.biolqm.metadata.constants;

import java.io.StringReader;
import java.io.StringWriter;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.InputStream;

public class XSLTransform {
	
	public static String simpleTransform(String source) {
        try {
			StringReader reader = new StringReader(source);
			StringWriter writer = new StringWriter();
			TransformerFactory tFactory = TransformerFactory.newInstance();
			
			InputStream is = XSLTransform.class
			  .getClassLoader()
			  .getResourceAsStream("html2markdown.xsl");
            Transformer transformer =
                tFactory.newTransformer(new StreamSource(is));

            transformer.transform(new StreamSource(reader),
                                  new StreamResult(writer));
								  
			return writer.toString();
        } catch (Exception e) {
            e.printStackTrace();
        }
		
		return null;
    }

/*     public static void setPropertiesXSLTransform() {
        //Set saxon as transformer.
        System.setProperty("javax.xml.transform.TransformerFactory", "net.sf.saxon.TransformerFactoryImpl");
    } */
}