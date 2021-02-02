package org.colomoto.biolqm.io.sbml;

import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ModelLayout;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseExporter;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;
import org.colomoto.biolqm.metadata.annotations.Metadata;

import org.sbml.jsbml.ASTNode;
import org.sbml.jsbml.ASTNode.Type;
import org.sbml.jsbml.Compartment;
import org.sbml.jsbml.SBMLDocument;
import org.sbml.jsbml.SBMLWriter;
import org.sbml.jsbml.ext.layout.*;
import org.sbml.jsbml.ext.qual.*;
import org.sbml.jsbml.SBase;
import org.sbml.jsbml.Annotation;
import org.sbml.jsbml.xml.XMLNode;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ArrayList;
import java.util.ArrayList;
import java.util.Set;

/**
 * SBML export using JSBML and the "qual" extension.
 *
 * @author Aurelien Naldi
 */
public class SBMLqualExport extends BaseExporter {

    private final ConnectivityMatrix matrix;
    private final MDDManager ddmanager;

    private final SBMLQualBundle qualBundle;

    private final List<NodeInfo> coreNodes;
    private final PathSearcher searcher;

    private Map<NodeInfo, QualitativeSpecies> node2species = new HashMap<NodeInfo, QualitativeSpecies>();
    private String[] coreIDS;
    private boolean needFilled = true;

    private String tr_prefix = "tr_";

    public SBMLqualExport(LogicalModel model) {
        this(model, model.hasLayout());
    }

    public SBMLqualExport(LogicalModel model, boolean addLayout) {
        super(model);

        this.ddmanager = model.getMDDManager();
        this.searcher = new PathSearcher(ddmanager, true);
        this.matrix = new ConnectivityMatrix(model);
        this.coreNodes = model.getComponents();

        this.qualBundle = SBMLqualHelper.newBundle(addLayout);
    }

    public void export() throws IOException {

        try {
            SBMLWriter writer = new SBMLWriter();
            writer.write(getSBMLDocument(), streams.output());
        } catch (XMLStreamException e) {
            throw new IOException(e);
        }
    }

    public SBMLDocument getSBMLDocument() {
        return getSBMLBundle().document;
    }

    public SBMLQualBundle getSBMLBundle() {
        ensureFilled();
        return qualBundle;
    }

    /**
     * Make sure that transition IDs can not conflict with species IDs
     */
    private void ensureTransitionPrefix() {
        for (NodeInfo ni: coreNodes) {
            String curID = ni.getNodeID();
            while (curID.startsWith(tr_prefix)) {
                tr_prefix += "_";
            }
        }
        for (NodeInfo ni: model.getExtraComponents()) {
            String curID = ni.getNodeID();
            while (curID.startsWith(tr_prefix)) {
                tr_prefix += "_";
            }
        }
    }

    public synchronized void ensureFilled() {
        if (needFilled) {
            ensureTransitionPrefix();

            needFilled = false;
            // add a compartment
            Compartment comp1 = qualBundle.model.createCompartment("comp1");
            comp1.setConstant(true);

            // add qualitative species
            List<NodeInfo> nodes = coreNodes;
            coreIDS = new String[coreNodes.size()];
            int[] functions = model.getLogicalFunctions();
            for (int i=0 ; i<functions.length ; i++) {
                NodeInfo ni = nodes.get(i);
                String curID = ni.getNodeID();
                coreIDS[i] = curID;

                QualitativeSpecies sp = qualBundle.qmodel.createQualitativeSpecies(curID, comp1);
                sp.setMaxLevel( ni.getMax());
                node2species.put(ni, sp);

                String name = ni.getName();
                if (name != null && name.length() > 0) {
                    sp.setName(name);
                }

                if (ni.isInput()) {
                    sp.setConstant(true);
                    // TODO: check consistency between function and input role?
                } else {
                    sp.setConstant(false);
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

                String curID = ni.getNodeID();
                QualitativeSpecies sp = qualBundle.qmodel.createQualitativeSpecies(curID, comp1);
                sp.setConstant(false);
                node2species.put(ni, sp);
                if (ni.isInput()) {
                    sp.setConstant(true);
                }

                // add its transition
                addTransition(ni, function, matrix.getRegulators(i, true));
                i++;
            }

            // Add layout information if available
            if (model.hasLayout()) {
                ModelLayout mlayout = model.getLayout();
                Layout layout = new Layout();
                layout.setId("__layout__");
                qualBundle.lmodel.addLayout(layout);

                double width = 0;
                double height = 0;
                for (NodeInfo ni: model.getComponents()) {
                    ModelLayout.LayoutInfo li = mlayout.getInfo(ni);
                    int x = li.x;
                    int w = li.width;
                    int y = li.y;
                    int h = li.height;
                    String id = getSpecies(ni).getId();
                    GeneralGlyph glyph = new GeneralGlyph();
                    glyph.setReference(id);
                    glyph.setId("_ly_"+id);
                    BoundingBox bb = new BoundingBox();
                    Point pos = bb.createPosition();
                    pos.setX(x);
                    pos.setY(y);
                    Dimensions dim = bb.createDimensions();
                    dim.setWidth(w);
                    dim.setHeight(h);

                    if (x+w > width) {
                        width = x + w;
                    }
                    if (y+h > height) {
                        height = y + h;
                    }
                    glyph.setBoundingBox(bb);
                    layout.addGeneralGlyph(glyph);
                }
                Dimensions dims = new Dimensions();
                dims.setWidth(width);
                dims.setHeight(height);
                layout.setDimensions(dims);

            }
			
			// add the annotations from the SBML model
			this.exportAllMetadata();
			System.out.println("Beware: only the official instances of dcterms will be saved (the qualifiers 'creator', 'created' and 'modified'). Moreover, nested annotations in dcterms as well as dcterms in nested annotations cannot be saved in SBML." + "\n");
        }
    }

    /**
     * Apply an initial condition to the exported model.
     *
     * @param state the initial levels for each core component (negative values for unspecified)
     */
    public void setInitialCondition(byte[] state) {
        ensureFilled();

        for (int idx=0 ; idx<state.length ; idx++) {
            int v = state[idx];
            if (v < 0) {
                continue;
            }

            NodeInfo ni = coreNodes.get(idx);
            QualitativeSpecies species = getSpecies(ni);
            if (species != null) {
                species.setInitialLevel(v);
            }
        }
    }

    public QualitativeSpecies getSpecies(NodeInfo ni) {
        return node2species.get(ni);
    }

    private void addTransition(NodeInfo ni, int function, int[] regulators) {

        String trID = tr_prefix+ni.getNodeID();
        Transition tr = qualBundle.qmodel.createTransition(trID+"_");
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
	
	private void exportElementMetadata(SBase element, Metadata metadata, String type) {
		
		if (metadata.isMetadataNotEmpty()) {
			Annotation annotation = metadata.getSBMLOfMetadata();
			
			if (!annotation.isEmpty()) {
				if (!type.equals("model")) {
					element.setMetaId("meta_"+element.getId());
				}
				element.setAnnotation(annotation);
			}
		}
		if (metadata.getNotes() != "") {
			try {
				XMLNode xmlnode = XMLNode.convertStringToXMLNode(metadata.getNotes());
				element.setNotes(xmlnode);
			} catch (XMLStreamException e) {
				System.err.println("Error exporting one of the notes in sbml." + "\n");
			}
		}
	}
	
	private void exportAllMetadata() {
		
		Metadata metadataModel = this.model.getMetadataOfModel();
		
		if (metadataModel.isMetadataNotEmpty() || metadataModel.getNotes() != "") {
			SBase elementModel = (SBase) qualBundle.document.getModel();
			exportElementMetadata(elementModel, metadataModel, "model");
		}
		
		for (Map.Entry<NodeInfo, QualitativeSpecies> entry : this.node2species.entrySet()) {
			NodeInfo node = entry.getKey();
			
			if (this.model.isSetMetadataOfNode(node)) {
				Metadata metadataSpecies = this.model.getMetadataOfNode(node);
				
				if (metadataSpecies.isMetadataNotEmpty() || metadataSpecies.getNotes() != "") {
					SBase elementSpecies = (SBase) entry.getValue();
					exportElementMetadata(elementSpecies, metadataSpecies, "species");
				}
			}
		}
	}
}
