package org.colomoto.biolqm.metadata.annotations;

import javax.xml.transform.*;
import javax.xml.transform.stream.*;
import java.io.File;
import java.io.StringReader;
import java.io.StringWriter;

public class XSLReader {

	public static String convertHTMLToMarkdown(String theHTML) throws TransformerException, TransformerConfigurationException {
		
        File xslFile = new File("src/main/resources/markdown.xsl");

        Source xmlSource = new StreamSource(new StringReader(theHTML));
        Source xsltSource = new StreamSource(xslFile);

        TransformerFactory transFact =
                TransformerFactory.newInstance();
        Transformer trans = transFact.newTransformer(xsltSource);

        StringWriter result = new StringWriter();
        trans.transform(xmlSource, new StreamResult(result));
		
		return result.toString();
	}
}
