package org.colomoto.biolqm.service;

/**
 * Simple service description interface.
 * Implement this interface to integrate a service which can be used in script mode
 *
 * @author Aurelien Naldi
 */
public interface Service {

    /**
     * get the ID of the service.
     * @return the service ID
     */
    String getID();

    /**
     * @return a list of alias names, or null if none
     */
    String[] getAliases();

    /**
     * Get a longer name for the service.
     * This is descriptive only and has no real role.
     * @return the service name
     */
    String getName();

    /**
     * Provide a brief description of parameters for the help message
     *
     * @return a short String describing the service parameters
     */
    String getDescription();

    /**
     * Precise type of support for multivalued models.
     *
     * @return the type of support: native, booleanized or none
     */
    MultivaluedSupport getMultivaluedSupport();

}
