package org.colomoto.biolqm.modifier.most_permissive;

import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.mddlib.IndexMapper;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDMapper;
import org.colomoto.mddlib.internal.MDDStoreImpl;

import java.util.*;

/**
 *
 * @author
 */
public class MostPermissiveModifier extends BaseModifier implements IndexMapper {
    private final LogicalModel model;
    private MDDManager ddm, newDDM;
    private List<NodeInfo> core, extra, newCore, newExtra;
    private int[] coreFunctions, extraFunctions, newCoreFunctions, newExtraFunctions;


    private MDDMapper mapper = null;
    private int curTarget = 0;
    private int nbBuffers = 0;

    public MostPermissiveModifier(LogicalModel model) {
        this.model = model;
    }

    @Override
    public LogicalModel performTask() {

        this.ddm = model.getMDDManager();
        this.core = model.getComponents();
        this.extra = model.getExtraComponents();
        this.coreFunctions = model.getLogicalFunctions();
        this.extraFunctions = model.getExtraLogicalFunctions();


        List<NodeInfo> extended_components = new ArrayList<>();
        for (NodeInfo ni: core) {
            extended_components.add(new NodeInfo(ni.getNodeID()+"_a"));
            extended_components.add(new NodeInfo(ni.getNodeID()+"_b"));
            extended_components.add(new NodeInfo(ni.getNodeID()+"_c"));
        }

        // new MDD manager with the extended variables
        byte[] new_state = new byte[3*coreFunctions.length];
        MDDManager new_ddmanager = new MDDStoreImpl(extended_components, 2);

        return null;
    }

    @Override
    public int get(int i) {
        return i ; }
}


