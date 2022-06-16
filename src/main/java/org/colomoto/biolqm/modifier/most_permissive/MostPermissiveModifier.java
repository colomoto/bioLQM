import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.mddlib.IndexMapper;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDMapper;
import org.colomoto.mddlib.MDDVariable;
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

    }


