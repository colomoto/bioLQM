package org.colomoto.biolqm.metadata.annotations;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.File;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

public class XSLTransform {

    /**
     * Simple transformation method.
     * @param sourcePath - Absolute path to source xml file.
     * @param xsltPath - Absolute path to xslt file.
     * @param resultDir - Directory where you want to put resulting files.
     */
/*     public static void simpleTransform(String sourcePath, String resultDir) {
        TransformerFactory tFactory = TransformerFactory.newInstance();
        try {
            Transformer transformer =
                tFactory.newTransformer(new StreamSource(new File("src/main/resources/html2markdown.xsl")));

            transformer.transform(new StreamSource(new File(sourcePath)),
                                  new StreamResult(new File(resultDir)));
        } catch (Exception e) {
            e.printStackTrace();
        }
    } */
	
	public static String simpleTransform(String source) {
        try {
			StringReader reader = new StringReader(source);
			StringWriter writer = new StringWriter();
			TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer transformer =
                tFactory.newTransformer(new StreamSource(new File("src/main/resources/html2markdown.xsl")));

            transformer.transform(new StreamSource(reader),
                                  new StreamResult(writer));
								  
			return writer.toString();
        } catch (Exception e) {
            e.printStackTrace();
        }
		
		return null;
    }

    public static void setPropertiesXSLTransform() {
        //Set saxon as transformer.
        System.setProperty("javax.xml.transform.TransformerFactory", "net.sf.saxon.TransformerFactoryImpl");
    }
}