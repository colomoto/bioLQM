package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.service.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.StreamProvider;

import java.io.IOException;

abstract public class PNFormat extends AbstractFormat {

    PNFormat(String uid, String descr) { super(uid, descr, MultivaluedSupport.MULTIVALUED); }

    abstract AbstractPNEncoder getEncoder(LogicalModel model);
    
    public PNConfig getConfig() {
    	return new PNConfig();
    }
    
    public void export(LogicalModel model, PNConfig config, StreamProvider out) throws IOException {
        if (config == null) {
        	config = new PNConfig();
        }
        AbstractPNEncoder encoder = getEncoder(model);
        encoder.export(config, out);
    }
    
    @Override
    public void export(LogicalModel model, StreamProvider out) throws IOException {
    	export(model, null, out);
    }

    @Override
    public boolean canExport() {
        return true;
    }
}
