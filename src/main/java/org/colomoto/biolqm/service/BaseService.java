package org.colomoto.biolqm.service;

/**
 * Common base class for all services, to store the associated metadata
 *
 * @author Aurelien Naldi
 */
public class BaseService implements Service {

    private final String id, name, descr;
    private final String[] aliases;
    private final MultivaluedSupport mvsupport;


    public BaseService(String id, String name, String descr, MultivaluedSupport mvsupport) {
        this(id, null, name, descr, mvsupport);
    }
    public BaseService(String id, String[] aliases, String name, String descr, MultivaluedSupport mvsupport) {
        this.id = id;
        this.name = name;
        this.descr = descr;
        this.aliases = aliases;
        this.mvsupport = mvsupport;
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

    @Override
    public MultivaluedSupport getMultivaluedSupport() {
        return mvsupport;
    }
}
