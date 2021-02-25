package org.colomoto.biolqm.io.sbml;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLStreamException;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.ModelLayout;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseLoader;
import org.colomoto.biolqm.metadata.annotations.Metadata;
import org.colomoto.biolqm.metadata.constants.XSLTransform;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDOperator;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.sbml.jsbml.ASTNode;
import org.sbml.jsbml.ASTNode.Type;
import org.sbml.jsbml.Annotation;
import org.sbml.jsbml.CVTerm;
import org.sbml.jsbml.Creator;
import org.sbml.jsbml.History;
import org.sbml.jsbml.ListOf;
import org.sbml.jsbml.SBase;
import org.sbml.jsbml.ext.layout.BoundingBox;
import org.sbml.jsbml.ext.layout.Dimensions;
import org.sbml.jsbml.ext.layout.GeneralGlyph;
import org.sbml.jsbml.ext.layout.GraphicalObject;
import org.sbml.jsbml.ext.layout.Layout;
import org.sbml.jsbml.ext.layout.Point;
import org.sbml.jsbml.ext.qual.FunctionTerm;
import org.sbml.jsbml.ext.qual.Input;
import org.sbml.jsbml.ext.qual.Output;
import org.sbml.jsbml.ext.qual.OutputTransitionEffect;
import org.sbml.jsbml.ext.qual.QualitativeSpecies;
import org.sbml.jsbml.ext.qual.Transition;
import org.sbml.jsbml.xml.XMLNode;

/**
 * Crude SBML import using JSBML and the qual extension.
 *
 * @author Aurelien Naldi
 */
public class SBMLqualImport extends BaseLoader {

    private SBMLQualBundle qualBundle = null;

    private Map<String, Integer> identifier2index;
    private MDDVariable[] ddvariables;

    private Map<String, Input> m_curInputs = new HashMap<String, Input>();


    public SBMLQualBundle getQualBundle() {
        return qualBundle;
    }

    public LogicalModel performTask() throws Exception {

        try {
            this.qualBundle = SBMLqualHelper.parseInputStream(streams.input());
        } catch (XMLStreamException e) {
            throw new IOException(e);
        }

        if (qualBundle == null) {
            return null;
        }

        identifier2index = new HashMap<>();


        List<NodeInfo> variables = getVariables();
        MDDManager ddmanager;
        MDDVariableFactory mvf = new MDDVariableFactory();
        for (NodeInfo ni: variables) {
            mvf.add(ni, (byte)(ni.getMax()+1));
        }
        ddmanager = MDDManagerFactory.getManager(mvf, 10);
        ddvariables = ddmanager.getAllVariables();
        int[] functions = new int[variables.size()];


        for (Transition tr: qualBundle.qmodel.getListOfTransitions()) {

            // get available inputs
            m_curInputs.clear();
            ListOf<Input> inputs = tr.getListOfInputs();
            for (Input input: inputs) {
                String inputID = input.getId();
                if (inputID != null) {
                    m_curInputs.put(inputID, input);
                }
            }


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
                mdd = MDDBaseOperators.OR.combine(ddmanager, mdd, f);
            }


            // apply it to outputs
            ListOf<Output> outputs = tr.getListOfOutputs();
            for (Output output: outputs) {
                OutputTransitionEffect effect = output.getTransitionEffect();
                if (effect != OutputTransitionEffect.assignmentLevel) {
                    throw new RuntimeException("Only handles assignement functions");
                }

                String name = output.getQualitativeSpecies();
                int idx = getIndexForName(name);
                NodeInfo ni = variables.get(idx);
                if (ni.isInput()) {
                    throw new RuntimeException("Constants can not be used as transition output");
                }
                functions[idx] = mdd;
            }

        }

        // add default functions for inputs
        int idx = 0;
        for (NodeInfo ni: variables) {
            if (ni.isInput()) {
                MDDVariable var = ddmanager.getVariableForKey(ni);
                int max = ni.getMax();
                if (max == 1) {
                    functions[idx] = var.getNode(0, 1);
                } else {
                    int[] values = new int[max+1];
                    for (int i=0 ; i<values.length ; i++) {
                        values[i] = i;
                    }
                    functions[idx] = var.getNode(values);

                }
            }
            idx++;
        }
        LogicalModel model = new LogicalModelImpl(variables, ddmanager, functions);

        // Load the layout information if available
        if (qualBundle.lmodel != null) {
            ListOf<Layout> layouts = qualBundle.lmodel.getListOfLayouts();
            if (layouts != null && layouts.size() > 0) {
                ModelLayout llayout = model.getLayout();
                Layout layout = layouts.get(0);

                for (GraphicalObject graphics: layout.getListOfAdditionalGraphicalObjects()) {
                    if (!(graphics instanceof GeneralGlyph)) {
                        continue;
                    }
                    GeneralGlyph glyph = (GeneralGlyph)graphics;
                    String sid = glyph.getReference();
                    NodeInfo ni = model.getComponent(sid);
                    if (ni == null) {
                        continue;
                    }
                    if (glyph.isSetBoundingBox()) {
                        BoundingBox bb = glyph.getBoundingBox();
                        if (bb.isSetPosition()) {
                            Point pos = bb.getPosition();
                            ModelLayout.LayoutInfo li = llayout.setPosition(ni, (int) pos.getX(), (int) pos.getY());
                            if (bb.isSetDimensions()) {
                                Dimensions dim = bb.getDimensions();
                                li.width = (int) dim.getWidth();
                                li.height = (int) dim.getHeight();
                            }
                        }
                    }
                }
            }
        }
		
		// load the annotations from the SBML model
		try {
			this.importAllMetadata(model, variables);
		} catch (XMLStreamException e) {
			System.err.println("Error importing model annotations." + "\n");
		}

        return model;
    }

    private List<NodeInfo> getVariables() {
        List<NodeInfo> variables = new ArrayList<NodeInfo>();
        int curIndex = 0;
        for (QualitativeSpecies sp: qualBundle.qmodel.getListOfQualitativeSpecies()) {
            String spid = sp.getId();
            if (spid.startsWith("s_")) {
                // remove prefix from ID if possible
                spid = spid.substring(2);
            }

            String name = sp.isSetName() ? sp.getName() : null;
            byte max = sp.isSetMaxLevel() ? (byte)sp.getMaxLevel() : (byte)-1;
            NodeInfo ni = new NodeInfo(spid, name, max);
            if (sp.isSetConstant() && sp.getConstant()) {
                ni.setInput(true);
            }
            variables.add(ni);
            identifier2index.put(sp.getId(), curIndex);
            curIndex++;
        }

        // fill missing max values
        guessMaxs(variables);

        return variables;
    }

    /**
     * If needed, guess the max level for species which did not specify it.
     *
     * @param variables
     */
    private void guessMaxs(List<NodeInfo> variables) {

        boolean needMax[] = new boolean[variables.size()];
        byte[] maxs = new byte[needMax.length];
        boolean allDefined = true;
        int i=0;
        for (NodeInfo ni: variables) {
            byte max = ni.getMax();
            maxs[i] = max;
            if (max < 0) {
                needMax[i] = true;
                maxs[i] = 1;
                allDefined = false;
            } else {
                needMax[i] = false;
            }
            i++;
        }

        if (allDefined) {
            return;
        }

        for (Transition tr: qualBundle.qmodel.getListOfTransitions()) {
            for (Output output: tr.getListOfOutputs()) {
                String name = output.getQualitativeSpecies();
                int idx = getIndexForName(name);
                if (!needMax[idx]) {
                    continue;
                }

                OutputTransitionEffect effect = output.getTransitionEffect();
                if (effect != OutputTransitionEffect.assignmentLevel) {
                    continue;
                }

                // parse terms for new max value
                int max = maxs[idx];
                for (FunctionTerm ft: tr.getListOfFunctionTerms()) {
                    int value = ft.getResultLevel();
                    if (value > max) {
                        max = value;
                    }
                }
                maxs[idx] = (byte)max;

            }

        }

        i=0;
        for (NodeInfo ni: variables) {
            if (needMax[i]) {
                ni.setMax(maxs[i]);
            }
            i++;
        }

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
     * @return
     */
    private int getMDDForMathML(MDDManager ddmanager, ASTNode math, int value) {

        Type type = math.getType();

        switch (type) {

            case NAME:
                String name = math.getName().trim();
                int threshold = 1;

                Input input = m_curInputs.get(name);
                if (input != null) {
                    name = input.getQualitativeSpecies().trim();
                    threshold = input.getThresholdLevel();
                }

                if (threshold < 1) {
                    // not really a constraint!
                    return value;
                }

                int index = getIndexForName(name);
                MDDVariable var = ddvariables[index];
                if (threshold >= var.nbval) {
                    throw new RuntimeException("Invalid threshold in "+input);
                }

                if (var.nbval == 2 ) {
                    return var.getNode(0, value);
                }
                int[] children = new int[var.nbval];
                for (int i=threshold ; i< var.nbval ; i++) {
                    children[i] = value;
                }
                return var.getNode(children);

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
                if (math.getChildCount() != 1) {
                    throw new RuntimeException("Invalid number of children in relation: "+math);
                }
                ASTNode child = math.getChild(0);
                int mdd = getMDDForMathML(ddmanager, child, value);
                return ddmanager.not(mdd);
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
            throw new RuntimeException("Invalid number of children in relation: "+relation);
        }
        ASTNode varNode = relation.getChild(0);
        ASTNode valueNode = relation.getChild(1);

        String varName = varNode.getName().trim();
        Integer relValue = null;
        boolean reversed = false;

        // extract content from children (NAME and INTEGER only)
        if (varNode.getType() == Type.NAME && valueNode.getType() == Type.INTEGER) {
            relValue = valueNode.getInteger();
        } else if (varNode.getType() == Type.INTEGER && valueNode.getType() == Type.NAME) {
            reversed = true;
            varName = valueNode.getName().trim();
            relValue = varNode.getInteger();
        } else if (varNode.getType() == Type.NAME && valueNode.getType() == Type.NAME) {
            String valueName = valueNode.getName().trim();
            Input input = m_curInputs.get(valueName);
            if (input == null) {
                // try reversing the relation
                input = m_curInputs.get(varName);
                if (input != null) {
                    reversed = true;
                    String stmp = varName;
                    varName = valueName;
                    valueName = stmp;
                }
            }

            if (input != null) {
                if (!varName.equals(input.getQualitativeSpecies().trim())) {
                    throw new RuntimeException("Constraint '"+input.getQualitativeSpecies().trim()+"' and variable '"+varName+"' do not match in: "+relation);
                }
                try {
                    relValue = input.getThresholdLevel();
                } catch (Exception e) {
                    relValue = 1;
                }
            }
        }

        if (relValue == null) {
            throw new RuntimeException("Could not find a value in: "+relation);
        }

        // handle inequalities in reversed relations ( "1 > g2" becomes "g2 < 1" )
        if (reversed) {
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

        int index = getIndexForName(varName);
        if (index < 0) {
            throw new RuntimeException("Unrecognized name in relation: "+relation);
        }

        MDDVariable var = ddvariables[index];


        // normalise inequalities and handle border cases (always true or false)
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

    private String mathml2string(ASTNode math, int value) {
        StringBuffer sb = new StringBuffer();
        mathml2string(math, sb);
        return sb.toString();
    }

    private void mathml2string(ASTNode math, StringBuffer sb) {

        Type type = math.getType();
        switch (type) {

            case NAME:
                String name = math.getName().trim();
                int threshold = 1;

                Input input = m_curInputs.get(name);
                if (input != null) {
                    name = input.getQualitativeSpecies().trim();
                    threshold = input.getThresholdLevel();
                }

                if (threshold < 1) {
                    // not really a constraint!
                    throw new RuntimeException("Inconsistent formula");
                }

                int index = getIndexForName(name);
                MDDVariable var = ddvariables[index];
                if (threshold >= var.nbval) {
                    throw new RuntimeException("Invalid threshold in "+input);
                }

                sb.append(var);
                if (threshold > 1) {
                    sb.append(var+":"+threshold);
                }
                return;

            case RELATIONAL_GEQ:
            case RELATIONAL_GT:
            case RELATIONAL_LEQ:
            case RELATIONAL_LT:
            case RELATIONAL_NEQ:
            case RELATIONAL_EQ:
                fillRelationString(math, sb);
                return;

            case CONSTANT_FALSE:
                sb.append("false");
                return;
            case CONSTANT_TRUE:
                sb.append("true");
                return;

            case LOGICAL_NOT:
                if (math.getChildCount() != 1) {
                    throw new RuntimeException("Invalid number of children in relation: "+math);
                }

                ASTNode child = math.getChild(0);
                switch (child.getType()) {
                    case CONSTANT_FALSE:
                        sb.append("true");
                        return;
                    case CONSTANT_TRUE:
                        sb.append("false");
                        return;
                    case LOGICAL_NOT:
                        if (child.getChildCount() != 1) {
                            throw new RuntimeException("Invalid number of children in relation: "+math);
                        }
                        mathml2string(child.getChild(0), sb);
                        return;
                    case NAME:
                        sb.append("!");
                        mathml2string(child, sb);
                        return;
                    default:
                        sb.append("!(");
                        mathml2string(child, sb);
                        sb.append(")");
                }
                return;
        }


        // now we should have a logical operation or some unrecognised MathML...
        String op = null;
        switch (type) {

            case LOGICAL_AND:
                op = " & ";
                break;

            case LOGICAL_OR:
                op = " | ";
                break;

            default:
                throw new RuntimeException("TODO: support MathML node for: "+math);
        }

        // if we get here, we have a recognised logical operation, hooray!
        // start by recursively identifying children!
        List<ASTNode> children = math.getChildren();
        boolean first = true;
        for (ASTNode child: children) {
            mathml2string(child, sb);
            if (!first) {
                first = false;
                sb.append(op);
            }
        }
    }

    private void fillRelationString(ASTNode relation, StringBuffer sb) {

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
            throw new RuntimeException("Invalid number of children in relation: "+relation);
        }
        ASTNode varNode = relation.getChild(0);
        ASTNode valueNode = relation.getChild(1);

        String varName = varNode.getName().trim();
        Integer relValue = null;
        boolean reversed = false;

        // extract content from children (NAME and INTEGER only)
        if (varNode.getType() == Type.NAME && valueNode.getType() == Type.INTEGER) {
            relValue = valueNode.getInteger();
        } else if (varNode.getType() == Type.INTEGER && valueNode.getType() == Type.NAME) {
            reversed = true;
            varName = valueNode.getName().trim();
            relValue = varNode.getInteger();
        } else if (varNode.getType() == Type.NAME && valueNode.getType() == Type.NAME) {
            String valueName = valueNode.getName().trim();
            Input input = m_curInputs.get(valueName);
            if (input == null) {
                // try reversing the relation
                input = m_curInputs.get(varName);
                if (input != null) {
                    reversed = true;
                    String stmp = varName;
                    varName = valueName;
                    valueName = stmp;
                }
            }

            if (input != null) {
                if (!varName.equals(input.getQualitativeSpecies().trim())) {
                    throw new RuntimeException("Constraint '"+input.getQualitativeSpecies().trim()+"' and variable '"+varName+"' do not match in: "+relation);
                }
                try {
                    relValue = input.getThresholdLevel();
                } catch (Exception e) {
                    relValue = 1;
                }
            }
        }

        if (relValue == null) {
            throw new RuntimeException("Could not find a value in: "+relation);
        }

        // handle inequalities in reversed relations ( "1 > g2" becomes "g2 < 1" )
        if (reversed) {
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

        int index = getIndexForName(varName);
        if (index < 0) {
            throw new RuntimeException("Unrecognized name in relation: "+relation);
        }

        MDDVariable var = ddvariables[index];

        // Normalise inequalities and handle border cases (always true or false)
        switch (type) {

            case RELATIONAL_GT:
                type = Type.RELATIONAL_GEQ;
                relValue += 1;
            case RELATIONAL_GEQ:
                if (relValue <= 0) {
                    sb.append("true");
                    return;
                }
                if (relValue >= var.nbval) {
                    sb.append("false");
                    return;
                }
                break;


            case RELATIONAL_LEQ:
                type = Type.RELATIONAL_LT;
                relValue += 1;
            case RELATIONAL_LT:
                if (relValue >= var.nbval) {
                    sb.append("true");
                    return;
                }
                if (relValue <= 0) {
                    sb.append("false");
                    return;
                }
                break;


            case RELATIONAL_NEQ:
                if (relValue < 0 || relValue >= var.nbval) {
                    sb.append("true");
                    return;
                }
                break;

            case RELATIONAL_EQ:
                if (relValue < 0 || relValue >= var.nbval) {
                    sb.append("false");
                    return;
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
                    sb.append("!"+var);
                    return;

                case RELATIONAL_GEQ:
                    sb.append(var);
                    return;

                case RELATIONAL_EQ:
                    if (relValue == 0) {
                        sb.append("!"+var);
                        return;
                    }
                    sb.append(var);
                    return;

                case RELATIONAL_NEQ:
                    if (relValue == 0) {
                        sb.append(var);
                        return;
                    }
                    sb.append("!"+var);
                    return;
            }

            throw new RuntimeException("Could not handle relation: "+relation);
        }

        throw new RuntimeException("Multi-valued is not handled here!");
    }
	
	private void importElementCVTerm(CVTerm cvterm, Metadata metadata) throws Exception {
		
		String qualifier = cvterm.getQualifier().getElementNameEquivalent();
		if (qualifier.equals("unknownQualifier") || qualifier.equals("isRelatedTo")) {
			qualifier = cvterm.getUnknownQualifierName();
		}
		
		int alternative = metadata.getNumberOfAlternatives(qualifier);
		if (alternative != 0) {
			alternative = metadata.createAlternative(qualifier);
		}
		
		// we add all the uris for this qualifier
		for (String uri: cvterm.getResources()) {			
			if (uri.indexOf("identifiers.org/") != -1) {
				uri = uri.split("identifiers.org/")[1];
			}
			
			int colon = uri.indexOf(':');
			int slash = uri.indexOf('/');
			
			int index = colon;
			if (colon == -1 || (slash != -1 && slash < colon)) {
				index = slash;
			}
			
			String collection = uri.substring(0, index);
			String identifier = uri.substring(index+1);
			
			metadata.addURI(qualifier, alternative, collection, identifier);
		}
		
		// and then we add the nested annotation recursively
		if (cvterm.isSetListOfNestedCVTerms()) {
			Metadata metadataNested = metadata.getMetadataOfQualifier(qualifier, alternative);
			
			for (CVTerm cvtermNested: cvterm.getListOfNestedCVTerms()) {
				this.importElementCVTerm(cvtermNested, metadataNested);
			}
		}
	}
		
    private void importElementHistory(Annotation annotation, Metadata metadata) throws Exception {
		
		if (annotation.isSetHistory()) {		
			History history = annotation.getHistory();
			
			String pattern = "yyyy-MM-dd";
			SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
						
			if (history.isSetCreatedDate()) { metadata.addDate("created", simpleDateFormat.format(history.getCreatedDate())); }
			if (history.isSetModifiedDate()) { metadata.addDate("modified", LocalDate.now().toString()); }
			
			// we don't use the old modifiedDate for the "modified" qualifier because we use the current date
			// it's okay because this change will affect the model only if it is saved, indicating it has indeed been modified
			// metadata.addDate("modified", simpleDateFormat.format(history.getModifiedDate()));
			
			for (Creator creator: history.getListOfCreators()) {
				String email = null;
				if (creator.isSetEmail()) { email = creator.getEmail(); }
				String organisation = null;
				if (creator.isSetOrganisation()) { organisation = creator.getOrganisation(); }
				metadata.addAuthor("creator", creator.getGivenName(), creator.getFamilyName(), email, organisation, null);
			}
		}
	}
	
	private void importElementTagsAndKeys(XMLNode xml, Metadata metadata) throws Exception {
	
		for (XMLNode qualifier: xml.getChildElements("qualifier", "uri_colomoto")) {
			String qualifierName = qualifier.getAttributes().getValue("name");

			for (XMLNode alternative: qualifier.getChildElements("alternative", "uri_colomoto")) {
				int alternativeNumber = Integer.parseInt(alternative.getAttributes().getValue("number"));
				if (alternativeNumber < 0) {
					alternativeNumber = metadata.createAlternative(qualifierName);
				}
				
				XMLNode tags = alternative.getChildElement("tags", "uri_colomoto");
				if (tags != null) {
					for (XMLNode tagNode: tags.getChildElements("tag", "uri_colomoto")) {
						
						String tag = tagNode.getChild(0).getCharacters();
						metadata.addTag(qualifierName, alternativeNumber, tag);
					}
				}
				
				XMLNode keys = alternative.getChildElement("keys", "uri_colomoto");
				if (keys != null) {
					for (XMLNode valuesNode: keys.getChildElements("values", "uri_colomoto")) {
						String key = valuesNode.getAttributes().getValue("key");
						String values = (String) valuesNode.getChild(0).getCharacters();
						List<String> valuesList = Arrays.asList(values.split(";;;"));
						
						for (String val: valuesList) {
							metadata.addKeyValue(qualifierName, alternativeNumber, key, val);
						}
					}
				}
				
				XMLNode nested = alternative.getChildElement("nested", "uri_colomoto");
				if (nested != null) {
					Metadata metadataQualifier = metadata.getMetadataOfQualifier(qualifierName, alternativeNumber);
					
					this.importElementTagsAndKeys(nested, metadataQualifier);
				}
			}
		}
	}
	
	private void importAllMetadata(LogicalModel model, List<NodeInfo> variables) throws Exception {
		
		SBase elementModel = (SBase) this.qualBundle.document.getModel();
		
		if (elementModel.isSetAnnotation() || elementModel.isSetNotes()) {
			Metadata metadataModel = model.getMetadataOfModel();
			
			if (elementModel.isSetAnnotation()) {
				Annotation annotationModel = elementModel.getAnnotation();
				
				// to deal with terms of bqbiol and bqmodel
				for (CVTerm cvterm: annotationModel.getListOfCVTerms()) {
					this.importElementCVTerm(cvterm, metadataModel);
				}
				
				// to deal with terms of dcterms
				this.importElementHistory(annotationModel, metadataModel);
				
				// to deal with tags and keys
				if (annotationModel.isSetOtherAnnotationThanRDF()) {		
					XMLNode nonRDFAnnotationModel = annotationModel.getNonRDFannotation().getChildElement("nonRDFAnnotation", "uri_colomoto");
					
					if (nonRDFAnnotationModel != null) {
						this.importElementTagsAndKeys(nonRDFAnnotationModel, metadataModel);
					}
				}
			}
			if (elementModel.isSetNotes()) {
				XMLNode notesModel = elementModel.getNotes();
				
				// to suppress the xmlns of the html language
				for (XMLNode content: notesModel.getChildElements("", "")) {
					content.clearNamespaces();
				}
				
				String htmlModel = notesModel.toXMLString();
				String markdownModel = XSLTransform.simpleTransform(htmlModel);
				metadataModel.setNotes(markdownModel);
			}
		}
		
		for (QualitativeSpecies elementSpecies: this.qualBundle.qmodel.getListOfQualitativeSpecies()) {
			
			if (elementSpecies.isSetAnnotation() || elementSpecies.isSetNotes()) {
				NodeInfo node = variables.get(this.getIndexForName(elementSpecies.getId()));
				Metadata metadataSpecies = model.getMetadataOfNode(node);
				
				if (elementSpecies.isSetAnnotation()) {					
					Annotation annotationSpecies = elementSpecies.getAnnotation();
						
					// to deal with terms of bqbiol and bqmodel
					for (CVTerm cvterm: annotationSpecies.getListOfCVTerms()) {
						this.importElementCVTerm(cvterm, metadataSpecies);
					}
					
					// to deal with terms of dcterms
					this.importElementHistory(annotationSpecies, metadataSpecies);
						
					// to deal with tags and keys
					if (annotationSpecies.isSetOtherAnnotationThanRDF()) {		
						XMLNode nonRDFAnnotationSpecies = annotationSpecies.getNonRDFannotation().getChildElement("nonRDFAnnotation", "uri_colomoto");
						
						if (nonRDFAnnotationSpecies != null) {
							this.importElementTagsAndKeys(nonRDFAnnotationSpecies, metadataSpecies);
						}
					}
				}
				if (elementSpecies.isSetNotes()) {
					XMLNode notesSpecies = elementSpecies.getNotes();
					notesSpecies.clearNamespaces();
				
					// to suppress the xmlns of the html language
					for (XMLNode content: notesSpecies.getChildElements("", "")) {
						content.clearNamespaces();
					}
				
					String htmlSpecies = notesSpecies.toXMLString();
					String markdownSpecies = XSLTransform.simpleTransform(htmlSpecies);
					metadataSpecies.setNotes(markdownSpecies);
				}
			}
		}
	}
}
