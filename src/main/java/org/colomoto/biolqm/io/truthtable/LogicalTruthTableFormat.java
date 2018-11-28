package org.colomoto.biolqm.io.truthtable;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class LogicalTruthTableFormat extends AbstractFormat {

    public static final String ID = "ltt";

    public LogicalTruthTableFormat() {
        super(ID, "Logical Truth Table format", MultivaluedSupport.MULTIVALUED);
    }

    @Override
    public LogicalTruthTableExport getExporter(LogicalModel model) {
        return new LogicalTruthTableExport(model);
    }


    @Override
    public LogicalTruthTableImport getLoader() {
        return new LogicalTruthTableImport();
    }
}
