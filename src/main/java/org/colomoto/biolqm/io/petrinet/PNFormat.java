package org.colomoto.biolqm.io.petrinet;

import java.io.IOException;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;

abstract public class PNFormat extends AbstractFormat {

    public PNFormat(String uid, String descr) { super(uid, descr, MultivaluedSupport.MULTIVALUED); }

    abstract AbstractPNEncoder getEncoder(LogicalModel model);
    
    public PNConfig getConfig() {
    	return new PNConfig();
    }
    
    public void export(LogicalModel model, PNConfig config, OutputStreamProvider out) throws IOException {
        if (config == null) {
        	config = new PNConfig();
        }
        AbstractPNEncoder encoder = getEncoder(model);
        encoder.export(config, out);
    }
    
    @Override
    public void export(LogicalModel model, OutputStreamProvider out) throws IOException {
    	export(model, null, out);
    }
}
