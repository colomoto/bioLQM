package org.colomoto.biolqm.modifier.most_permissive;

import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.mddlib.IndexMapper;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDMapper;

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

        return null;
    }

    @Override
    public int get(int i) {
        return i ; }
}


