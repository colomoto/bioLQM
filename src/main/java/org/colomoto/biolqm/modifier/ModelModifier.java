package org.colomoto.biolqm.modifier;

import org.colomoto.biolqm.service.ModelTask;

/**
 * Model Modifiers are tasks dedicated to the construction of modified logical models.
 *
 * <ul>
 *     <li>Use the call() method to retrieve the modified model directly.</li>
 *     <li>Use the background(listener) method to launch it in a separate thread.</li>
 * </ul>
 *
 * @author Aurelien Naldi
 */
public interface ModelModifier extends ModelTask {

}
