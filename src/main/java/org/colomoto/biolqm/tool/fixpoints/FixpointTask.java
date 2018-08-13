package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.settings.state.StateList;
import org.colomoto.biolqm.tool.AbstractToolTask;
import org.colomoto.common.task.AbstractTask;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;

import java.util.List;

public class FixpointTask extends AbstractToolTask<FixpointList> {

    public FixpointMethod method = FixpointMethod.MDD;
    public boolean pattern = false;
    public boolean extra = false;


    public FixpointTask(LogicalModel model) {
        super(model);
    }

    public void setParameters(String[] parameters) {
        if (parameters == null) {
            return;
        }

        for (String p: parameters) {
            p = p.trim();
            if ("asp".equalsIgnoreCase(p )) {
                this.method = FixpointMethod.ASP;
            }
            if ("pattern".equalsIgnoreCase(p )) {
                this.pattern = true;
            }
            if ("extra".equalsIgnoreCase(p )) {
                this.extra = true;
            }
        }
    }

    @Override
    protected FixpointList performTask() throws Exception {
        FixpointList result = null;
        switch (method) {
            case ASP:
                result = getASP(model);
                break;
            default:
                result = getMDD(model, pattern);
                break;
        }

        result.setExtra(extra);
        return result;
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

    @Override
    public void cli() {
        StateList result = null;
        try {
            result = call();
        } catch(Exception e) {
            System.out.println("Error while constructing the result");
            e.printStackTrace();
            return;
        }

        if (result == null || result.size() < 1) {
            System.out.println("NO RESULTS");
            return;
        }

        // print out the result
        NodeInfo[] components = result.getComponents();
        for (NodeInfo ni : components) {
            System.out.print(ni + " ");
        }
        System.out.println();

        int nrows = result.size();
        for (int row=0 ; row<nrows ; row++) {
            for (int col=0 ; col<components.length ; col++) {
                int i = result.get(row, col);
                if (i == -5) {
                    System.out.print("?");
                } else if (i<0) {
                    System.out.print("-");
                } else {
                    System.out.print(i);
                }
            }
            System.out.println();
        }
    }


}
