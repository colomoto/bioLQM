package org.colomoto.logicalmodel.modifier;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.services.Service;

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
     * Provide a brief description of parameters for the help message
     *
     * @return a short String describing the service parameters
     */
    String getDescription();

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
     * @param model
     * @param parameters
     * @return
     */
    LogicalModel getModifiedModel(LogicalModel model, String parameters);

}
