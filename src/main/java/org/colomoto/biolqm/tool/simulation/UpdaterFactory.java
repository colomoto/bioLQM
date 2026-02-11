package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.deterministic.*;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.CompleteUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.PriorityUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;

/**
 * Static helper class to construct simulation updaters based on a configuration string
 *
 * @author Aurelien Naldi
 */
public class UpdaterFactory {

    public static DeterministicUpdater getDeterministicUpdater(LogicalModel model, String s) {

        if (s == null || s.length() < 1 || s.trim().equalsIgnoreCase("synchronous")) {
            return new SynchronousUpdater(model);
        }

        String[] setup = s.split(" ", 2);
        if (setup[0].equalsIgnoreCase("sequential")) {
            if (setup.length < 2) {
                return new SequentialUpdater(model);
            }

            String param = setup[1].trim();
            // Identify simple sequential updaters
            if (!param.contains(":") && !param.contains("/") && !param.contains("[")) {
                return new SequentialUpdater(model, param);
            }
            return new BlockSequentialUpdater(model, param);
        }

        if (setup[0].equalsIgnoreCase("priority ")) {
            return new DeterministicPriorityUpdater(model, setup[1].trim());
        }

        throw new RuntimeException("Unrecognized updater: "+s);
    }

    public static MultipleSuccessorsUpdater getMultipleSuccessorUpdater(LogicalModel model, String s) {

        if (s == null || s.length() < 1) {
            return new AsynchronousUpdater(model);
        }

        String[] setup = s.trim().split(" ", 2);
        
        if (setup[0].equalsIgnoreCase("asynchronous")) {
            return new AsynchronousUpdater(model);
        } else if (setup[0].equalsIgnoreCase("complete")) {
            return new CompleteUpdater(model);
        }

        if (setup[0].equalsIgnoreCase("priority")) {
            if (setup.length == 1) {
                return new PriorityUpdater(model, null);
            }
            return new PriorityUpdater(model, setup[1]);
        }

        throw new RuntimeException("Unrecognized updater: "+s);
    }

    public static RandomUpdater getRandomUpdater(LogicalModel model, String s) {

        if (s == null || s.length() < 1) {
            return new RandomUpdaterWrapper(getMultipleSuccessorUpdater(model,s));
        }

        // TODO: parse custom random updaters definitions

        return new RandomUpdaterWrapper(getMultipleSuccessorUpdater(model,s));
    }
}
