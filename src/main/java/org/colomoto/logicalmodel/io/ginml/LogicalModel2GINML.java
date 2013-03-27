package org.colomoto.logicalmodel.io.ginml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.common.xml.XMLWriter;
import org.colomoto.logicalmodel.ConnectivityMatrix;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;
import org.colomoto.mddlib.VariableEffect;

/**
 * Encoder to transform a LogicalModel into a GINML file.
 *  
 * @author Aurelien Naldi
 */
public class LogicalModel2GINML extends AbstractGINMLWriter {

	private final LogicalModel model;
	private final MDDManager ddmanager;
	
	private final ConnectivityMatrix matrix;

	private final List<NodeInfo> nodes;
	List<NodeInfo> extraNodes;

	public LogicalModel2GINML(LogicalModel model) {
		this.model = model;
		this.ddmanager = model.getMDDManager();
		this.matrix = new ConnectivityMatrix(model);
		
		this.nodes = model.getNodeOrder();
		this.extraNodes = model.getExtraComponents();
	}
	
	@Override
	public List<String> getNodes() {
		List<String> nodeIDs = new ArrayList<String>(this.nodes.size()+extraNodes.size());
		for (NodeInfo ni: this.nodes) {
			nodeIDs.add(ni.getNodeID());
		}
		for (NodeInfo ni: this.extraNodes) {
			nodeIDs.add(ni.getNodeID());
		}
		return nodeIDs;
	}

	@Override
	public void writeNodes(XMLWriter out) throws IOException {
		writeNodes(out, false);
		writeNodes(out, true);
	}
	
	private void writeNodes(XMLWriter out, boolean extra) throws IOException {
		
		int[] functions;
		List<NodeInfo> nodes;
		if (extra) {
			functions = model.getExtraLogicalFunctions();
			nodes = extraNodes;
		} else {
			functions = model.getLogicalFunctions();
			nodes = this.nodes;
		}
		int i=0;
		for (NodeInfo ni: nodes) {
			int[] regulators = matrix.getRegulators(i, extra);
			VariableEffect[][] effects = matrix.getRegulatorEffects(i, extra);
			writeNode(out, ni, functions[i], regulators, effects);
			i++;
		}
	}		

	private void writeNode(XMLWriter xw, NodeInfo ni, int function, int[] regulators, VariableEffect[][] effects) throws IOException {

		String nodeID = ni.getNodeID();
		writeNodeDecl(xw, nodeID, ni.getMax());

		PathSearcher searcher = new PathSearcher(ddmanager);
		int[] path = searcher.getPath();
		int[] values = new int[regulators.length];
		searcher.setNode(function);
		for (int v: searcher) {
			if (v == 0) {
				continue;
			}
			
			writeLogicalParameters(xw, v, nodeID, path, regulators, effects, 0, values);
			for (int r:regulators) {
				int value = path[r];
			}
		}
		if (ni.isInput()) {
			xw.openTag("annotation");
			xw.openTag("comment");
			xw.addContent(ni.getNodeID() + " is an input node marked with an auto-regulation.");
			xw.closeTag();
			xw.closeTag();
		}
		xw.closeTag();
	}
	
	private void writeLogicalParameters(XMLWriter xw, int targetValue, String nodeID, int[] path, int[] regulators, VariableEffect[][] effects, int curIdx, int[] values) throws IOException {
		
		while (curIdx < regulators.length) {
			int regIdx = regulators[curIdx];
			int value = path[regIdx];
			if (value < 0) {
				int max = nodes.get(regIdx).getMax();
				for (int v=0 ; v<=max ; v++) {
					values[curIdx] = v;
					writeLogicalParameters(xw, targetValue, nodeID, path, regulators, effects, curIdx+1, values);
				}
				
				return;
			}
			
			values[curIdx] = value;
			curIdx++;
		}
		
		// build list of active regulators
		StringBuffer sPresent = new StringBuffer();
		boolean first = true;
		for (int i=0 ; i<regulators.length ; i++) {
			int curValue = values[i];
			if (curValue != 0) {
				if (effects[i][curValue-1] == VariableEffect.NONE) {
					return;
				}
				if (first) {
					first = false;
				} else {
					sPresent.append(" ");
				}
				sPresent.append( nodes.get( regulators[i] ).getNodeID() );
				sPresent.append(":");
				sPresent.append(nodeID);
				sPresent.append(":");
				sPresent.append(curValue);
			}
		}
		
		writeLogicalParameter(xw, targetValue, sPresent.toString());
	}

	@Override
	public void writeEdges(XMLWriter out) throws IOException {
		writeEdges(out, false);
		writeEdges(out, true);
	}
	
	private void writeEdges(XMLWriter xw, boolean extra) throws IOException {
		int[] functions;
		if (extra) { 
			functions = model.getExtraLogicalFunctions();
		} else {
			functions = model.getLogicalFunctions();
		}
		
		for (int idx=0 ; idx < functions.length ; idx++) {
			String id = nodes.get(idx).getNodeID();
			int[] regulators = matrix.getRegulators(idx, extra);
			VariableEffect[][] effects = matrix.getRegulatorEffects(idx, extra);
			if (regulators != null && regulators.length > 0) {
				writeEdges(xw, id, regulators, effects);
			}
		}
	}
	
	private void writeEdges(XMLWriter xw, String id, int[] regulators, VariableEffect[][] effects) throws IOException {
		
		for (int i=0 ; i<regulators.length ; i++) {
			int reg = regulators[i];
			VariableEffect[] curEffects = effects[i];
			for (int v=0 ; v < curEffects.length ; v++) {
				VariableEffect veffect = curEffects[v];
				if (veffect == VariableEffect.NONE) {
					continue;
				}
				String seffect = null;
				switch (veffect) {
				case POSITIVE:
					seffect = "positive";
					break;
				case NEGATIVE:
					seffect = "negative";
					break;
				case DUAL:
					seffect = "unknown";
					break;
				}
				writeEdge(xw, nodes.get(reg).getNodeID(), id, v+1, seffect);
			}
		}
	}

}
