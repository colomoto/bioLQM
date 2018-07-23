package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.AbstractToolTask;
import org.colomoto.common.task.AbstractTask;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;

import java.util.List;

public class FixpointTask extends AbstractToolTask<FixpointList> {

    public FixpointMethod method = FixpointMethod.MDD;
    public boolean pattern = false;


    public FixpointTask(LogicalModel model, String[] parameters) {
        super(model);

        for (String p: parameters) {
            p = p.trim();
            if ("asp".equalsIgnoreCase(p )) {
                this.method = FixpointMethod.ASP;
            }
            if ("pattern".equalsIgnoreCase(p )) {
                this.pattern = true;
            }
        }
    }

    @Override
    protected FixpointList doGetResult() throws Exception {
        switch (method) {
            case ASP:
                return getASP(model);
            default:
                return getMDD(model, pattern);
        }
    }

    public void useASP() {
        this.method = FixpointMethod.ASP;
    }

    public void useMDD() {
        this.method = FixpointMethod.MDD;
    }


    public FixpointList getMDD(LogicalModel model) {
        return getMDD(model, false);
    }

    private byte[] cloneState(int[] path) {
        byte[] result = new byte[path.length];
        for (int idx = 0 ; idx<path.length ; idx++) {
            result[idx] = (byte)path[idx];
        }
        return result;
    }

    public FixpointList getMDD(LogicalModel model, boolean pattern) {
        FixpointSearcher ssearcher = new FixpointSearcher(model);
        FixpointList result = new FixpointList(model);
        try {
            int stable = ssearcher.call();
            MDDManager ddm = ssearcher.getMDDManager();

            PathSearcher psearcher = new PathSearcher(ddm, 1);
            int[] path = psearcher.setNode(stable);
            if (pattern) {
                for (int v : psearcher) {
                    result.add(cloneState(path));
                }
                ddm.free(stable);
                return result;
            }

            // Find and expand undefined components in the results
            List<NodeInfo> components = model.getComponents();
            for (int v : psearcher) {
                byte[] max = cloneState(path);
                byte[] copy = null;
                for (int idx = 0; idx < max.length; idx++) {
                    if (max[idx] < 0) {
                        if (copy == null) {
                            copy = max.clone();
                        }
                        copy[idx] = 0;
                        max[idx] = components.get(idx).getMax();
                    }
                }
                if (copy == null) {
                    // fully defined state, no need to expand it
                    result.add(max);
                    continue;
                }

                boolean hasjokers = true;
                result.add(copy.clone());
                while (hasjokers) {
                    // We must have at least one unassigned component which can be increased:
                    // find it and check if another exists for the next round
                    hasjokers = false;
                    int idx = max.length - 1;
                    for ( ; idx >= 0 ; idx--) {
                        // ignore all assigned components
                        if (path[idx] >= 0) {
                            continue;
                        }

                        // Reset unassigned components that reached their max value
                        // They can be used for the next round
                        if (copy[idx] == max[idx]) {
                            copy[idx] = 0;
                            hasjokers = true;
                            continue;
                        }

                        //
                        copy[idx]++;
                        break;
                    }

                    // The next state was found, add it to the list
                    result.add(copy.clone());

                    if (hasjokers) {
                        // we already know that at least one other component can be increased
                        continue;
                    }

                    // Find at least one candidate for the next round
                    for ( ; idx >= 0 ; idx--) {
                        if (copy[idx] < max[idx]) {
                            // found a spot which can change later
                            hasjokers = true;
                            break;
                        }
                    }
                }
            }
            ddm.free(stable);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }

    private FixpointList getASP(LogicalModel model) {
        StableASP asp = new StableASP(model);
        return asp.get();
    }

}
