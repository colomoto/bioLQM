package org.colomoto.biolqm.service;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.common.task.Task;

/**
 * Model Tasks are tasks which return a model.
 *
 * <ul>
 *     <li>Use the call() method to retrieve the resulting model directly.</li>
 *     <li>Use the background(listener) method to launch it in a separate thread.</li>
 * </ul>
 *
 * @author Aurelien Naldi
 */
public interface ModelTask extends Task<LogicalModel> {

}
