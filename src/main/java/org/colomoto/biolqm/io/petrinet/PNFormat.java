package org.colomoto.biolqm.io.petrinet;

import org.colomoto.biolqm.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;

import java.io.IOException;

abstract public class PNFormat extends AbstractFormat {

    PNFormat(String uid, String descr) { super(uid, descr, MultivaluedSupport.MULTIVALUED); }

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
    public void exportImpl(LogicalModel model, OutputStreamProvider out) throws IOException {
    	export(model, null, out);
    }
}
