package org.colomoto.biolqm;

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
}
