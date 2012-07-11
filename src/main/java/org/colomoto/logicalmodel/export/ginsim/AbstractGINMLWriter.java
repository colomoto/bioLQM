package org.colomoto.logicalmodel.export.ginsim;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.common.xml.XMLWriter;
import org.colomoto.logicalmodel.ConnectivityMatrix;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;

abstract public class AbstractGINMLWriter {

    public static final String DEFAULT_DTD = "http://gin.univ-mrs.fr/GINsim/GINML_2_1.dtd";
	
	public void export(OutputStream out) throws IOException {
		
		XMLWriter xw = new XMLWriter(out, DEFAULT_DTD);
		
		xw.openTag("gxl");
		xw.openTag("graph");
		xw.addAttr("id", "defaultID");
		xw.addAttr("type", "regulatory");

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
	
	public abstract List<String> getNodes();
	public abstract void writeNodes(XMLWriter out) throws IOException;
	public abstract void writeEdges(XMLWriter out) throws IOException;
	
	
	protected void writeEdge(XMLWriter xw, String from, String to, int threshold, String sign) throws IOException {
		xw.openTag("edge");
		xw.addAttr("id", from+":"+to+":"+threshold);
		xw.addAttr("from", from);
		xw.addAttr("to", to);
		
		xw.addAttr("sign", sign);
		xw.addAttr("minvalue", ""+threshold);
		
		xw.closeTag();
	}
	
	protected void writeNodeDecl(XMLWriter xw, String nodeID, int max) throws IOException {
		xw.openTag("node");
		xw.addAttr("id", nodeID);
		xw.addAttr("maxvalue", ""+max);

	}
	
	protected void writeLogicalParameter(XMLWriter xw, int targetValue, String activeInteractions) throws IOException {
		xw.openTag("parameter");
		xw.addAttr("val", ""+targetValue);
		xw.addAttr("idActiveInteractions", activeInteractions);
		xw.closeTag();
	}
}
