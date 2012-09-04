package org.colomoto.logicalmodel.io.sbml;

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
import org.sbml.jsbml.SBMLDocument;
import org.sbml.jsbml.SBMLWriter;
import org.sbml.jsbml.ext.qual.FunctionTerm;
import org.sbml.jsbml.ext.qual.Input;
import org.sbml.jsbml.ext.qual.InputTransitionEffect;
import org.sbml.jsbml.ext.qual.Output;
import org.sbml.jsbml.ext.qual.OutputTransitionEffect;
import org.sbml.jsbml.ext.qual.QualitativeSpecies;
import org.sbml.jsbml.ext.qual.Sign;
import org.sbml.jsbml.ext.qual.Transition;

/**
 * SBML export using JSBML and the "qual" extension.
 * 
 * @author Aurelien Naldi
 */
public class SBMLqualExport {

	private final LogicalModel model;
	private final ConnectivityMatrix matrix;
	private final MDDManager ddmanager;
	
	private final SBMLQualBundle qualBundle;

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
		this.coreNodes = model.getNodeOrder();

		this.qualBundle = SBMLqualHelper.newBundle();
	}

	public void export(OutputStream out) throws IOException, XMLStreamException {

		SBMLWriter writer = new SBMLWriter();
		writer.write(getSBMLDocument(), out);
	}

	public SBMLDocument getSBMLDocument() {
		return getSBMLBundle().document;
	}
	
	public SBMLQualBundle getSBMLBundle() {
		ensureFilled();
		return qualBundle;
	}

	public synchronized void ensureFilled() {
		if (needFilled) {
			needFilled = false;
			// add a compartment
			Compartment comp1 = qualBundle.model.createCompartment("comp1");
	
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
				QualitativeSpecies sp = qualBundle.qmodel.createQualitativeSpecies(curID, comp1.getId(), isConstant);
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
				QualitativeSpecies sp = qualBundle.qmodel.createQualitativeSpecies(curID, comp1.getId(), isConstant);
				node2species.put(ni, sp);
				
				// add its transition
				addTransition(ni, function, matrix.getRegulators(i, true));
				i++;
			}
		}
	}
	
	public QualitativeSpecies getSpecies(NodeInfo ni) {
			return node2species.get(ni);
	}
	
	private void addTransition(NodeInfo ni, int function, int[] regulators) {
		if (ddmanager.isleaf(function)) {
			// FIXME: input-less transition or constant?
			return;
		}
		
		String trID = "tr_"+ni.getNodeID();
		Transition tr = qualBundle.qmodel.createTransition(trID);
		Output out = tr.createOutput(trID+"_out", node2species.get(ni), OutputTransitionEffect.assignmentLevel);
		
		for (int idx: regulators) {
			NodeInfo ni_reg = coreNodes.get(idx);
			Input in = tr.createInput(trID+"_in_"+idx, node2species.get(ni_reg), InputTransitionEffect.none);
			in.setSign(Sign.unknown);  // TODO: add proper sign
		}
		
		
		// real stuff: logical functions
		FunctionTerm[] fTerms = new FunctionTerm[ni.getMax()+1];
		
		// start with a default to 0
		FunctionTerm fterm = new FunctionTerm();
		fterm.setDefaultTerm(true);
		fterm.setResultLevel(0);
		fTerms[0] = fterm;
		
		// extract others from the actual functions
		ASTNode orNode = new ASTNode(ASTNode.Type.LOGICAL_OR);
		int[] path = searcher.setNode(function);
		for (int leaf: searcher) {
			if (leaf == 0) {
				continue;
			}
			
			// build a condition for this path
			ASTNode andNode = new ASTNode(ASTNode.Type.LOGICAL_AND);
			for (int i=0 ; i<path.length ; i++) {
				int cst = path[i];
				if (cst < 0) {
					continue;
				}
				// TODO: support intervals with GEQ and LT
				ASTNode constraintNode = new ASTNode(ASTNode.Type.RELATIONAL_EQ);
				constraintNode.addChild( new ASTNode(coreIDS[i]) );
				constraintNode.addChild( new ASTNode(cst) );
				andNode.addChild(constraintNode);
			}
			
			fterm = fTerms[leaf];
			if (fterm == null) {
				fterm = new FunctionTerm();
				fterm.setResultLevel(leaf);
				fTerms[leaf] = fterm;
			}
			
			// remove the and if only one constraint is defined
			if (andNode.getChildCount() == 1) {
				andNode = andNode.getChild(0);
			}
			
			orNode.addChild(andNode);
		}
		// remove the "or" layer if not needed
		if (orNode.getChildCount() == 1) {
			orNode = orNode.getChild(0);
		}
		fterm.setMath(orNode);
		
		// add all function terms
		for (FunctionTerm ft: fTerms) {
			if (ft != null) {
				tr.addFunctionTerm(ft);
			}
		}
	}
}
