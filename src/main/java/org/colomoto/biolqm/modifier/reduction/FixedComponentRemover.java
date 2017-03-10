package org.colomoto.biolqm.modifier.reduction;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.perturbation.RegulatorRemovalOperation;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

import java.util.List;

/**
 * Detect fixed components and propagate their fixed value to their targets
 *
 * @author Aurelien Naldi
 */
public class FixedComponentRemover {


    public static LogicalModel reduceFixed(LogicalModel model, boolean removeFixed) {

        MDDManager ddmanager = model.getMDDManager();
        int[] oldFunctions = model.getLogicalFunctions();
        int[] functions = new int[oldFunctions.length];
        for (int i=0 ; i<functions.length ; i++) {
            int f = oldFunctions[i];
            ddmanager.use(f);
            functions[i] = f;
        }
        int[] oldExtraFunctions = model.getExtraLogicalFunctions();
        int[] extraFunctions = new int[oldExtraFunctions.length];
        for (int i=0 ; i<extraFunctions.length ; i++) {
            int f = oldExtraFunctions[i];
            ddmanager.use(f);
            extraFunctions[i] = f;
        }

        boolean[] knownFixed = new boolean[ functions.length ];

        // detect and propagate fixed components until no new ones are found
        boolean changed = false;
        boolean hasNewFixed = true;
        while(hasNewFixed) {
            hasNewFixed = false;

            // detect new fixed components
            for (int i=0 ; i<functions.length ; i++) {
                int f = functions[i];
                if (!knownFixed[i] && ddmanager.isleaf(f)) {
                    knownFixed[i] = true;
                    hasNewFixed = true;

                    MDDVariable var = ddmanager.getAllVariables()[i];
                    RegulatorRemovalOperation op = new RegulatorRemovalOperation(ddmanager, var, f);

                    for (int j=0 ; j<functions.length ; j++) {
                        int curFunc = functions[j];
                        int newFunc = op.restrict( curFunc );
                        if (newFunc != curFunc) {
                            changed = true;
                            ddmanager.free(curFunc);
                            functions[j] = newFunc;
                        } else {
                            ddmanager.free(newFunc);
                        }
                    }
                    for (int j=0 ; j<extraFunctions.length ; j++) {
                        int curFunc = extraFunctions[j];
                        int newFunc = op.restrict( curFunc );
                        if (newFunc != curFunc) {
                            changed = true;
                            ddmanager.free(curFunc);
                            extraFunctions[j] = newFunc;
                        } else {
                            ddmanager.free(newFunc);
                        }
                    }
                }
            }

        }

        // construct an updated model if needed
        if (changed) {
            List<NodeInfo> core = model.getComponents();
        	if (removeFixed) {
        		for (int i = knownFixed.length - 1; i >= 0; i--) {
        			if (knownFixed[i]) {
        				core.remove(i);
        			}
        		}
        		int[] newFunctions = new int[core.size()];
        		for (int i = 0, n=0; i < knownFixed.length; i++) {
        			if (!knownFixed[i]) {
        				newFunctions[n] = functions[i];
        				n++;
        			}
        		}
        		functions = newFunctions;
        	}
            List<NodeInfo> extra = model.getExtraComponents();
            LogicalModel newModel = new LogicalModelImpl(ddmanager, core, functions, extra, extraFunctions);

            return newModel;
        }

        // the model was unchanged: free the copied functions
        for (int f: functions) {
            ddmanager.free(f);
        }
        for (int f: extraFunctions) {
            ddmanager.free(f);
        }
        return model;
    }

}
