package org.colomoto.logicalmodel.io.sbml;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLStreamException;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDOperator;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.sbml.jsbml.ASTNode;
import org.sbml.jsbml.ASTNode.Type;
import org.sbml.jsbml.ListOf;
import org.sbml.jsbml.ext.qual.FunctionTerm;
import org.sbml.jsbml.ext.qual.Output;
import org.sbml.jsbml.ext.qual.QualitativeSpecies;
import org.sbml.jsbml.ext.qual.Transition;

/**
 * Crude SBML import using JSBML and the qual extension.
 * 
 * @author Aurelien Naldi
 */
public class SBMLqualImport {

	private final SBMLQualBundle qualBundle;
	
	private Map<String, Integer> identifier2index;
	private MDDVariable[] ddvariables;

	
	public SBMLqualImport(File f) throws IOException, XMLStreamException {
		this.qualBundle = SBMLqualHelper.loadFile(f);
	}
	
	public SBMLQualBundle getQualBundle() {
		return qualBundle;
	}
	
	public LogicalModel getModel() {
		
		if (qualBundle == null) {
			return null;
		}
		
		ListOf<QualitativeSpecies> species = qualBundle.qmodel.getListOfQualitativeSpecies();
		identifier2index = new HashMap<String, Integer>();
		
		
		List<NodeInfo> variables = new ArrayList<NodeInfo>();
		int curIndex = 0;
		for (QualitativeSpecies sp: species) {
			String name = sp.getName();
			if (name == null || name.length() == 0) {
				name = sp.getId();
				if (name.startsWith("s_")) {
					// remove prefix from ID if possible
					name = name.substring(2);
				}
			}
			byte max = 1; // FIXME: (byte)sp.getMaxLevel()+1;
			
			NodeInfo ni = new NodeInfo(name, max);
			variables.add(ni);
			identifier2index.put(sp.getId(), curIndex);
			curIndex++;
		}

		// FIXME: this does not support multivalued cases for now
		MDDManager ddmanager = MDDManagerFactory.getManager(variables, 10);
		ddvariables = ddmanager.getAllVariables();
		int[] functions = new int[variables.size()];
		
		
		for (Transition tr: qualBundle.qmodel.getListOfTransitions()) {
			
			// look for default value
			int defaultValue = 0;
			for (FunctionTerm ft: tr.getListOfFunctionTerms()) {
				if (ft.isDefaultTerm()) {
					defaultValue = ft.getResultLevel();
					break;
				}
			}


			// build MDDs for the function terms
			int mdd = defaultValue;
			for (FunctionTerm ft: tr.getListOfFunctionTerms()) {
				int value = ft.getResultLevel();
				if (value == defaultValue) {
					continue;
				}
				
				ASTNode math = ft.getMath();
				int f = getMDDForMathML(ddmanager, ft.getMath(), value);

				// FIXME: rough workaround for now, needs more subtle solution
				int oldmdd = mdd;
				mdd = MDDBaseOperators.OR.combine(ddmanager, mdd, f);
				
				System.out.println("func: "+math+" --> "+f + ".  "+ oldmdd +"  becomes  " + mdd);
			}
			
			
			// apply it to outputs
			ListOf<Output> outputs = tr.getListOfOutputs();
			for (Output output: outputs) {
				output.getTransitionEffect();  // TODO: check the type of effect
				
				String name = output.getQualitativeSpecies();
				int idx = getIndexForName(name);
				functions[idx] = mdd;
			}

		}
		
		LogicalModel model = new LogicalModelImpl(variables, ddmanager, functions);
		return model;
	}
	
	/**
	 * Retrieve the model component corresponding to a SBML ID.
	 * @param name a SBML ID for a species
	 * @return the index of the corresponding component in the LogicalModel
	 */
	public int getIndexForName(String name) {
		
		Integer index = identifier2index.get(name);
		if (index == null) {
			throw new RuntimeException("Could not find ID: "+name);
		}
		
		return index;
	}
	
	/**
	 * Get a MDD representing a parsed MathML function.
	 * 
	 * @param ddmanager
	 * @param math
	 * @param value
	 * @param defaultValue
	 * @return
	 */
	private int getMDDForMathML(MDDManager ddmanager, ASTNode math, int value) {
		
		Type type = math.getType();

		switch (type) {
		
		case NAME:
			String name = math.getName();
			int index = getIndexForName(name);
			MDDVariable var = ddvariables[index];
			
			return var.getNode(0, value);

		case RELATIONAL_GEQ:
		case RELATIONAL_GT:
		case RELATIONAL_LEQ:
		case RELATIONAL_LT:
		case RELATIONAL_NEQ:
		case RELATIONAL_EQ:
			if (math.getChildCount() != 2) {
				throw new RuntimeException("Invalid relation: "+math);
			}

			ASTNode varNode = math.getChild(0);
			ASTNode valueNode = math.getChild(1);
			
			if (varNode.getType() != Type.NAME) {
				if (valueNode.getType() == Type.NAME) {
					varNode = valueNode;
					valueNode = math.getChild(0);
				} else {
					throw new RuntimeException("Invalid relation children: "+math);
				}
			}
			
			Type valueType = valueNode.getType();
			int relValue = -1;
			if (valueType == Type.INTEGER) {
				relValue = valueNode.getInteger();
			} else if (valueType == Type.NAME) {
				// TODO: support named values
				throw new RuntimeException("Constant relation values not yet supported: "+valueNode);
			} else {
				throw new RuntimeException("Unrecognised relation value: "+valueNode);
			}
			
			name = varNode.getName();
			index = getIndexForName(name);
			var = ddvariables[index];

			if (0 > relValue || var.nbval <= relValue) {
				throw new RuntimeException("Relation value out of [0.."+var.nbval+"[ range: "+valueNode);
			}

			
			switch (type) {
			
			case RELATIONAL_GEQ:
			case RELATIONAL_GT:
			case RELATIONAL_LEQ:
			case RELATIONAL_LT:
			case RELATIONAL_NEQ:
				// TODO: support more relations
				throw new RuntimeException("non-equal relations not yet supported: "+math);
				
			case RELATIONAL_EQ:
				if (var.nbval == 2) {
					if (relValue == 0) {
						return var.getNode(1, 0);
					}
					return var.getNode(0, 1);
				}
				
				// TODO: more complex constraints
				int[] values = new int[var.nbval];
				break;

			default:
				throw new RuntimeException("unknown relation type: "+math);
			}
			
			break;

		case CONSTANT_FALSE:
			return 0;
		case CONSTANT_TRUE:
			return value;

		case LOGICAL_NOT:
			throw new RuntimeException("NOT not yet supported: "+math);
		}

		
		// now we should have a logical operation or some unrecognised MathML...
		MDDOperator op = null;
		switch (type) {

		case LOGICAL_AND:
			op = MDDBaseOperators.AND;
			break;
			
		case LOGICAL_OR:
			op = MDDBaseOperators.OR;
			break;
			
		default:
			throw new RuntimeException("TODO: support MathML node for: "+math);
		}

		// if we get here, we have a recognised logical operation, hooray!
		// start by recursively identifying children!
		List<ASTNode> children = math.getChildren();
		int childCount = children.size();
		int[] childrenFunctions = new int[childCount];
		int i=0;
		for (ASTNode child: children) {
			childrenFunctions[i] = getMDDForMathML(ddmanager, child, value);
			i++;
		}


		// combine children
		switch (childCount) {
		case 0:
			throw new RuntimeException("Logical operation without children");
		case 1:
			return childrenFunctions[0];
		case 2:
			// probably the most common case
			return op.combine(ddmanager, childrenFunctions[0], childrenFunctions[1]);
		default:
			return op.combine(ddmanager, childrenFunctions);
		}
	}
	
	
	
	public static void main(String[] args) {
		try {
			new SBMLqualImport( new File("target/testExport.sbml")).getModel();
			
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
