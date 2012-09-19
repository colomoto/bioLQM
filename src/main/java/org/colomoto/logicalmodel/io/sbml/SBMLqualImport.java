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
import org.colomoto.mddlib.MDDVariableFactory;
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
		boolean isMultivalued = false;
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
			
			byte max = (byte)sp.getMaxLevel();
			NodeInfo ni = new NodeInfo(name, max);
			variables.add(ni);
			if (max > 2) {
				isMultivalued = true;
			}
			identifier2index.put(sp.getId(), curIndex);
			curIndex++;
		}

		MDDManager ddmanager;
		if (isMultivalued) {
			MDDVariableFactory mvf = new MDDVariableFactory();
			for (NodeInfo ni: variables) {
				mvf.add(ni, (byte)(ni.getMax()+1));
			}
			ddmanager = MDDManagerFactory.getManager(mvf, 10);
		} else {
			ddmanager = MDDManagerFactory.getManager(variables, 10);
		}
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
				if (math == null) {
					continue;
				}
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
			return getMDDForRelation(math, value);
			
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
	
	/**
	 * Parse a relation term and get a matching MDD.
	 * 
	 * @param relation an ASTNode corresponding to a relation
	 * @param value the value to return when the relation is satisfied
	 * @return a MDD testing this relation
	 */
	private int getMDDForRelation(ASTNode relation, int value) {
		
		Type type = relation.getType();
		
		// consistency check: should only be called for relation nodes
		switch (type) {
		case RELATIONAL_GEQ:
		case RELATIONAL_GT:
		case RELATIONAL_LEQ:
		case RELATIONAL_LT:
		case RELATIONAL_NEQ:
		case RELATIONAL_EQ:
			break;
		default:
			throw new RuntimeException("Not a relation: "+relation);
		}
		
		// a relation should always have two children
		if (relation.getChildCount() != 2) {
			throw new RuntimeException("Invalid relation: "+relation);
		}
		ASTNode varNode = relation.getChild(0);
		ASTNode valueNode = relation.getChild(1);

		
		// try to detect reversed relations ( "1 == g2" instead of "g2 == 1" )
		if (valueNode.getType() == Type.NAME && varNode.getType() == Type.INTEGER) {
			varNode = valueNode;
			valueNode = relation.getChild(0);

			// reverse inequalities as well
			switch (type) {
			case RELATIONAL_GEQ:
				type = Type.RELATIONAL_LEQ;
				break;
			case RELATIONAL_LEQ:
				type = Type.RELATIONAL_GEQ;
				break;
			case RELATIONAL_GT:
				type = Type.RELATIONAL_LT;
				break;
			case RELATIONAL_LT:
				type = Type.RELATIONAL_GT;
				break;
			}
		}

		
		// extract the variable and value from the children 
		// TODO: support named values
		if (varNode.getType() != Type.NAME || valueNode.getType() != Type.INTEGER) {
			throw new RuntimeException("Missing name or unsupported value in relation: "+relation);
		}
		
		String name = varNode.getName();
		int index = getIndexForName(name);
		if (index < 0) {
			throw new RuntimeException("Unrecognized name in relation: "+relation);
		}

		MDDVariable var = ddvariables[index];
		int relValue = valueNode.getInteger();
		
		
		// handle border cases, that are always true or false
		switch (type) {
		
		case RELATIONAL_GT:
			type = Type.RELATIONAL_GEQ;
			relValue += 1;
		case RELATIONAL_GEQ:
			if (relValue <= 0) {
				return value;
			}
			if (relValue >= var.nbval) {
				return 0;
			}
			break;

			
		case RELATIONAL_LEQ:
			type = Type.RELATIONAL_LT;
			relValue += 1;
		case RELATIONAL_LT:
			if (relValue >= var.nbval) {
				return value;
			}
			if (relValue <= 0) {
				return 0;
			}
			break;

			
		case RELATIONAL_NEQ:
			if (relValue < 0 || relValue >= var.nbval) {
				return value;
			}
			break;
			
		case RELATIONAL_EQ:
			if (relValue < 0 || relValue >= var.nbval) {
				return 0;
			}
			break;
			
		default:
			throw new RuntimeException("unknown relation type: "+relation);
		}

		
		
		// now we should have a valid relValue and only EQ, NEQ, GEQ or LT relations
		if (0 > relValue || var.nbval <= relValue) {
			throw new RuntimeException("Relation value out of [0.."+var.nbval+"[ range: "+valueNode);
		}

		
		if (var.nbval == 2) {
			switch (type) {
			
			case RELATIONAL_LT:
				return var.getNode(value, 0);
				
			case RELATIONAL_GEQ:
				return var.getNode(0, value);
				
			case RELATIONAL_EQ:
				if (relValue == 0) {
					return var.getNode(value, 0);
				}
				return var.getNode(0, value);

			case RELATIONAL_NEQ:
				if (relValue == 0) {
					return var.getNode(0, value);
				}
				return var.getNode(value, 0);
			}
			
			throw new RuntimeException("Could not handle relation: "+relation);
		}

		
		int[] values = new int[var.nbval];
		switch (type) {
		
		case RELATIONAL_GEQ:
			for (int v=relValue ; v<var.nbval ; v++) {
				values[v] = value;
			}
			return var.getNode(values);
		
		case RELATIONAL_LT:
			for (int v=0 ; v<relValue ; v++) {
				values[v] = value;
			}
			return var.getNode(values);

		case RELATIONAL_NEQ:
			for (int v=0 ; v<var.nbval ; v++) {
				if (v == relValue) {
					values[v] = 0;
				} else {
					values[v] = value;
				}
			}
			return var.getNode(values);
			
		case RELATIONAL_EQ:
			values[relValue] = value;
			return var.getNode(values);

		}
		
		throw new RuntimeException("Could not handle relation: "+relation);
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
