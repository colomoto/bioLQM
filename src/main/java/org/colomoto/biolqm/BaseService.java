package org.colomoto.biolqm;

/**
 * Common base class for all services, to store the associated metadata
 *
 * @author Aurelien Naldi
 */
public class BaseService implements Service {

    private final String id, name, descr;
    private final String[] aliases;


    public BaseService(String id, String name, String descr) {
        this(id, null, name, descr);
    }
    public BaseService(String id, String[] aliases, String name, String descr) {
        this.id = id;
        this.name = name;
        this.descr = descr;
        this.aliases = aliases;
    }

    @Override
    public String getID() {
        return id;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getDescription() {
        return descr;
    }

    @Override
    public String[] getAliases() {
        return aliases;
    }

    @Override
    public String toString() {
        return getID() +"\t"+ getName();
    }
}
