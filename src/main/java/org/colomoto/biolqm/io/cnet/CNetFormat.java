package org.colomoto.biolqm.io.cnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.truthtable.TruthTableImport;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class CNetFormat extends AbstractFormat {

    public static final String ID = "cnet";

    public CNetFormat() {
        super(ID, "cnet functions format", MultivaluedSupport.BOOLEANIZED);
    }

    @Override
    public CNetExport getExporter(LogicalModel model) {
        return new CNetExport(model);
    }


    @Override
    public CNetImport getLoader() {
        return new CNetImport();
    }
}
