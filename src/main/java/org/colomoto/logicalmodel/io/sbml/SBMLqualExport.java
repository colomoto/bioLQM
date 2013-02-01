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
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;
import org.colomoto.mddlib.VariableEffect;
import org.sbml.jsbml.ASTNode;
import org.sbml.jsbml.ASTNode.Type;
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
		this.searcher = new PathSearcher(ddmanager, true);
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
				
				QualitativeSpecies sp = qualBundle.qmodel.createQualitativeSpecies(curID, comp1);
				sp.setMaxLevel( ni.getMax());
				node2species.put(ni, sp);
				
				if (ni.isInput()) {
					sp.setConstant(true);
					// TODO: check consistency between function and input role?
				}
			}
			
			// add transitions
			for (int i=0 ; i<functions.length ; i++) {
				NodeInfo ni = nodes.get(i);
				if (!ni.isInput()) {
					addTransition(nodes.get(i), functions[i], matrix.getRegulators(i, false));
				}
			}
			
			// add species and transitions for extra nodes as well
			nodes = model.getExtraComponents();
			functions = model.getExtraLogicalFunctions();
			for (int i=0 ; i<functions.length ; i++) {
				NodeInfo ni = nodes.get(i);
				int function = functions[i];

				String curID = "s_"+ni.getNodeID();
				QualitativeSpecies sp = qualBundle.qmodel.createQualitativeSpecies(curID, comp1);
				node2species.put(ni, sp);
				if (ni.isInput()) {
					sp.setConstant(true);
				}
				
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
		
		String trID = "tr_"+ni.getNodeID();
		Transition tr = qualBundle.qmodel.createTransition(trID);
		tr.createOutput(trID+"_out", node2species.get(ni), OutputTransitionEffect.assignmentLevel);
		
		if (ddmanager.isleaf(function)) {
			// only add a default term
			FunctionTerm fterm = new FunctionTerm();
			fterm.setDefaultTerm(true);
			fterm.setResultLevel(function);
			tr.addFunctionTerm(fterm);
			return;
		}
		
		for (int idx: regulators) {
			NodeInfo ni_reg = coreNodes.get(idx);
			Input in = tr.createInput(trID+"_in_"+idx, node2species.get(ni_reg), InputTransitionEffect.none);
			
			// determine the sign of the regulation
			Sign sign = Sign.unknown;
			MDDVariable regVar = ddmanager.getVariableForKey(ni_reg);
			switch (ddmanager.getVariableEffect(regVar, function)) {
			case DUAL:
				sign = Sign.dual;
				break;
			case POSITIVE:
				sign = Sign.positive;
				break;
			case NEGATIVE:
				sign = Sign.negative;
				break;
			}
			in.setSign(sign);
		}
		
		
		// start with a default to 0
		FunctionTerm fterm = new FunctionTerm();
		fterm.setDefaultTerm(true);
		fterm.setResultLevel(0);
		tr.addFunctionTerm(fterm);
		
		// extract others from the actual functions
		ASTNode[] orNodes = new ASTNode[ni.getMax()+1];
		int[] path = searcher.setNode(function);
		int[] tmax = searcher.getMax();
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
				
				int max = tmax[i];
				if (max >= 0 && max < cst) {
					System.err.println("############## wrong max?");
					continue;
				}
				
				if (max == cst) {
					// constrain to a single value
					ASTNode constraintNode = new ASTNode(ASTNode.Type.RELATIONAL_EQ);
					constraintNode.addChild( new ASTNode(coreIDS[i]) );
					constraintNode.addChild( new ASTNode(cst) );
					andNode.addChild(constraintNode);
				} else {
					// constrain to a range
					if (cst > 0) {
						ASTNode constraintNode = new ASTNode(ASTNode.Type.RELATIONAL_GEQ);
						constraintNode.addChild( new ASTNode(coreIDS[i]) );
						constraintNode.addChild( new ASTNode(cst) );
						andNode.addChild(constraintNode);
					}
					
					if (max > 0) {
						ASTNode constraintNode = new ASTNode(ASTNode.Type.RELATIONAL_LEQ);
						constraintNode.addChild( new ASTNode(coreIDS[i]) );
						constraintNode.addChild( new ASTNode(max) );
						andNode.addChild(constraintNode);
					}
				}
			}
			
			// remove the and if only one constraint is defined
			if (andNode.getChildCount() == 1) {
				andNode = andNode.getChild(0);
			}

			
			ASTNode orNode = orNodes[leaf];
			if (orNode == null) {
				orNodes[leaf] = andNode;
			} else {
				if (orNode.getType() != Type.LOGICAL_OR) {
					ASTNode oldOrNode = orNode;
					orNode = new ASTNode(Type.LOGICAL_OR);
					orNode.addChild(oldOrNode);
					orNodes[leaf] = orNode;
				}
				orNode.addChild(andNode);
			}
			
		}
		
		// add all function terms
		for (int level=1 ; level<orNodes.length ; level++) {
			ASTNode math = orNodes[level];
			if (math == null) {
				continue;
			}
			FunctionTerm ft = new FunctionTerm();
			ft.setResultLevel(level);
			ft.setMath(math);
			tr.addFunctionTerm(ft);
		}
	}
}
