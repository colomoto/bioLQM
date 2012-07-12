package org.colomoto.logicalmodel.export.sbml;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLStreamException;

import org.colomoto.logicalmodel.ConnectivityMatrix;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;
import org.sbml.jsbml.ASTNode;
import org.sbml.jsbml.Compartment;
import org.sbml.jsbml.Model;
import org.sbml.jsbml.SBMLDocument;
import org.sbml.jsbml.SBMLWriter;
import org.sbml.jsbml.ext.layout.LayoutConstants;
import org.sbml.jsbml.ext.qual.FunctionTerm;
import org.sbml.jsbml.ext.qual.Input;
import org.sbml.jsbml.ext.qual.InputTransitionEffect;
import org.sbml.jsbml.ext.qual.Output;
import org.sbml.jsbml.ext.qual.OutputTransitionEffect;
import org.sbml.jsbml.ext.qual.QualConstant;
import org.sbml.jsbml.ext.qual.QualitativeModel;
import org.sbml.jsbml.ext.qual.QualitativeSpecies;
import org.sbml.jsbml.ext.qual.Sign;
import org.sbml.jsbml.ext.qual.TemporisationType;
import org.sbml.jsbml.ext.qual.Transition;

public class SBMLqualExport {

	private final LogicalModel model;
	private final ConnectivityMatrix matrix;
	private final MDDManager ddmanager;
	
	private final SBMLDocument sdoc;
	private final Model smodel;
	private final QualitativeModel qmodel;

	private final List<NodeInfo> coreNodes;
	private final PathSearcher searcher;
	
	private Map<NodeInfo, QualitativeSpecies> node2species = new HashMap<NodeInfo, QualitativeSpecies>();
	private String[] coreIDS;
	private boolean needFilled = true;
	
	public SBMLqualExport(LogicalModel model) {
		this.model = model;
		this.ddmanager = model.getMDDManager();
		this.searcher = new PathSearcher(ddmanager);
		this.matrix = new ConnectivityMatrix(model);

		// init SBML document
		sdoc = new SBMLDocument(3,1);
		sdoc.addNamespace(QualConstant.shortLabel, "xmlns", QualConstant.namespaceURI);
		sdoc.addNamespace(LayoutConstants.shortLabel, "xmlns", LayoutConstants.namespaceURI);

		// create SBML and qual models
		smodel = sdoc.createModel("model_id");
		qmodel = new QualitativeModel(smodel);
		smodel.addExtension(QualConstant.namespaceURI, qmodel);
		
		coreNodes = model.getNodeOrder();
	}

	public void export(OutputStream out) throws IOException, XMLStreamException {

		SBMLWriter writer = new SBMLWriter();
		writer.write(getSBMLDocument(), out);
	}

	
	public SBMLDocument getSBMLDocument() {
		if (needFilled) {
			// add a compartment
			Compartment comp1 = smodel.createCompartment("comp1");
	
			// add qualitative species
			List<NodeInfo> nodes = coreNodes;
			coreIDS = new String[coreNodes.size()];
			int[] functions = model.getLogicalFunctions();
			for (int i=0 ; i<functions.length ; i++) {
				NodeInfo ni = nodes.get(i);
				String curID = "s_"+ni.getNodeID();
				coreIDS[i] = curID;
				boolean isConstant = ddmanager.isleaf(functions[i]);
				
				// TODO: set boundary condition for inputs
				// TODO: set value for constant components
				QualitativeSpecies sp = qmodel.createQualitativeSpecies(curID, false, comp1.getId(), isConstant);
				node2species.put(ni, sp);
			}
			
			// add transitions
			for (int i=0 ; i<functions.length ; i++) {
				addTransition(nodes.get(i), functions[i], matrix.getRegulators(i, false));
			}
	
			
			// add species and transitions for extra nodes as well
			nodes = model.getExtraComponents();
			functions = model.getExtraLogicalFunctions();
			for (int i=0 ; i<functions.length ; i++) {
				NodeInfo ni = nodes.get(i);
				int function = functions[i];
				boolean isConstant = ddmanager.isleaf(function);
				
				// TODO: set boundary condition for inputs (should not happen here?)
				// TODO: set value for constant components
				String curID = "s_"+ni.getNodeID();
				QualitativeSpecies sp = qmodel.createQualitativeSpecies(curID, false, comp1.getId(), isConstant);
				node2species.put(ni, sp);
				
				// add its transition
				addTransition(ni, function, matrix.getRegulators(i, true));
				i++;
			}
		}

		return sdoc;
	}
	
	private void addTransition(NodeInfo ni, int function, int[] regulators) {
		if (ddmanager.isleaf(function)) {
			// FIXME: input-less transition or constant?
			return;
		}
		
		String trID = "tr_"+ni.getNodeID();
		Transition tr = qmodel.createTransition(trID);
		tr.setTemporisationType(TemporisationType.priority);  // FIXME: really??
		Output out = tr.createOutput(trID+"_out", node2species.get(ni), OutputTransitionEffect.assignmentLevel);
		
		for (int idx: regulators) {
			NodeInfo ni_reg = coreNodes.get(idx);
			Input in = tr.createInput(trID+"_in_"+idx, node2species.get(ni_reg), InputTransitionEffect.none);
			in.setSign(Sign.unknown);  // TODO: add proper sign
		}
		
		
		// real stuff: logical functions
		FunctionTerm[] fTerms = new FunctionTerm[ni.getMax()+1];
		System.out.println(fTerms.length);
		
		// start with a default to 0
		FunctionTerm fterm = new FunctionTerm();
		fterm.setDefaultTerm(true);
		fterm.setResultLevel(0);
		fTerms[0] = fterm;
		
		// extract others from the actual functions
		int[] path = searcher.setNode(function);
		for (int leaf: searcher) {
			if (leaf == 0) {
				continue;
			}
			
			// build a condition for this path
			ASTNode andNode = new ASTNode(ASTNode.Type.LOGICAL_AND);
			for (int i=0 ; i<path.length ; i++) {
				int cst = path[i];
				if (cst >= 0) {
					andNode.addChild( new ASTNode(coreIDS[i]));
				}
			}
			
			fterm = fTerms[leaf];
			if (fterm == null) {
				fterm = new FunctionTerm();
				fterm.setResultLevel(leaf);
				fTerms[leaf] = fterm;
			}
			
			// add this condition and create parent if needed
			ASTNode math = fterm.getMath();
			if (math == null) {
				fterm.setMath(andNode);
			} else if (math.getType() == ASTNode.Type.LOGICAL_OR) {
				math.addChild(andNode);
			} else {
				ASTNode or = new ASTNode(ASTNode.Type.LOGICAL_OR);
				or.addChild(math);
				or.addChild(andNode);
			}
		}
		
		// add all function terms
		for (FunctionTerm ft: fTerms) {
			if (ft != null) {
				tr.addFunctionTerm(ft);
			}
		}
	}
}
