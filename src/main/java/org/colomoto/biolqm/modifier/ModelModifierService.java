package org.colomoto.biolqm.modifier;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.Service;

/**
 * A service providing modifiers for logical models.
 * Such services should be registered for service discovery.
 * To provide a generic interface, the services can construct new modifiers
 * by parsing a String (provided by the user on the command-line or in a script).
 * Implementors can provide additional methods with specialised API to setup the modifiers.
 *
 * @author Aurelien Naldi
 */
public interface ModelModifierService extends Service {

    /**
     * Setup a new modifier object.
     *
     * @param model the model to modify
     * @param parameters a string to setup the modifier if applicable
     *
     * @return a configured modifier instance
     */
    ModelModifier getModifier(LogicalModel model, String parameters);

    /**
     * Setup a modifier and directly retrieve the modified model.
     * This is a shorthand for getModifier(model,parameters).getModifiedModel()
     *
     * @param model the model to be modified
     * @param parameters the setting String (can be empty for some modifiers)
     * @return a new modified model
     */
    LogicalModel getModifiedModel(LogicalModel model, String parameters);

}
