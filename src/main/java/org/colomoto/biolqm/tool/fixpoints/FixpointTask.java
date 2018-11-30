package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.state.PatternStateIterator;
import org.colomoto.biolqm.helper.state.StateList;
import org.colomoto.biolqm.tool.AbstractToolTask;
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

    @Override
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

            // The Pattern iterator will be used to expand patterns into individual states if needed
            PatternStateIterator expander = null;
            if (!pattern) {
                List<NodeInfo> components = model.getComponents();
                byte[] max = new byte[components.size()];
                for (int i=0 ; i<max.length ; i++) {
                    max[i] = components.get(i).getMax();
                }
                expander = new PatternStateIterator(max, max);
            }

            // Enumerate all paths in the result MDD
            PathSearcher psearcher = new PathSearcher(ddm, 1);
            int[] path = psearcher.setNode(stable);
            for (int v : psearcher) {
                if (pattern) {
                    result.add(cloneState(path));
                } else {
                    expander.reset(path);
                    while (expander.hasNext()) {
                        result.add(expander.next().clone());
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
