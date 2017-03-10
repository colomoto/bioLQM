package org.colomoto.biolqm.io.ginml;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import org.colomoto.common.xml.XMLWriter;

/**
 * Some common methods to write a GINML file.
 * 
 * @author Aurelien Naldi
 */
abstract public class AbstractGINMLWriter {

    public static final String DEFAULT_DTD = "http://gin.univ-mrs.fr/GINsim/GINML_2_1.dtd";

    /**
     * Start the GINML export.
     * 
	 * @param out an opened output stream to save the result
	 * @throws IOException if writing fails
     */
	public void export(OutputStream out) throws IOException {
		
		XMLWriter xw = new XMLWriter(out, DEFAULT_DTD);
		
		xw.openTag("gxl");
		xw.openTag("graph");
		xw.addAttr("id", "defaultID");
		xw.addAttr("class", "regulatory");

		StringBuffer nodeOrder = new StringBuffer();
		List<String> nodes = getNodes();
		for (String nodeID: nodes) {
			nodeOrder.append(" ");
			nodeOrder.append(nodeID);
		}
		nodeOrder.replace(0, 1, "");
		
		xw.addAttr("nodeorder", nodeOrder.toString());

		writeNodes(xw);
		writeEdges(xw);
		
		// close the graph and gxl tags
		xw.closeTag();
		xw.closeTag();
		
		// close the writer and stream
		xw.close();
	}
	
	/**
	 * get the names of the nodes to save in GINML.
	 * 
	 * @return the list of node IDs
	 */
	public abstract List<String> getNodes();
	
	/**
	 * Start writing the nodes. This must be implemented by subclasses.
	 * 
	 * @param out an opened XML writer to save the result
	 * @throws IOException if writing fails
	 */
	public abstract void writeNodes(XMLWriter out) throws IOException;
	
	/**
	 * Start writing the edges (interactions). This must be implemented by subclasses.
	 * 
	 * @param out an opened XML writer to save the result
	 * @throws IOException if writing fails
	 */
	public abstract void writeEdges(XMLWriter out) throws IOException;
	
	
	/**
	 * Write an edge to GINML.
	 * 
	 * @param xw an opened XML writer to save the result
	 * @param from the name of the source node
	 * @param to the name of the target node
	 * @param threshold the threshold of this interaction
	 * @param sign the sign of this interaction
	 * @throws IOException if writing fails
	 */
	protected void writeEdge(XMLWriter xw, String from, String to, int threshold, String sign) throws IOException {
		xw.openTag("edge");
		xw.addAttr("id", from+":"+to+":"+threshold);
		xw.addAttr("from", from);
		xw.addAttr("to", to);
		
		xw.addAttr("sign", sign);
		xw.addAttr("minvalue", ""+threshold);
		
		xw.closeTag();
	}
	
	/**
	 * Write a node declaration to GINML.
	 * This method will not close the tag to let the caller add child elements, especially parameters.
	 * 
	 * @param xw an opened XML writer to save the result
	 * @param nodeID the ID of the component to save
	 * @param max the maximal value of this component (should be at least 1)
	 * @throws IOException if writing fails
	 */
	protected void writeNodeDecl(XMLWriter xw, String nodeID, int max) throws IOException {
		xw.openTag("node");
		xw.addAttr("id", nodeID);
		xw.addAttr("maxvalue", ""+max);

	}
	
	/**
	 * Write a logical parameter to GINML.
	 * 
	 * @param xw an opened XML writer to save the result
	 * @param targetValue the target value when this parameter is enabled
	 * @param activeInteractions the list of associated active interactions
	 * @throws IOException if writing fails
	 */
	protected void writeLogicalParameter(XMLWriter xw, int targetValue, String activeInteractions) throws IOException {
		xw.openTag("parameter");
		xw.addAttr("val", ""+targetValue);
		if (!activeInteractions.isEmpty()) {
			xw.addAttr("idActiveInteractions", activeInteractions);
		}
		xw.closeTag();
	}
}
