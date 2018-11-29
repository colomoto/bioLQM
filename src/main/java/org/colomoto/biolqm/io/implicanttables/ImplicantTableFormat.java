package org.colomoto.biolqm.io.implicanttables;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.AbstractFormat;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.service.MultivaluedSupport;
import org.kohsuke.MetaInfServices;

@MetaInfServices(LogicalModelFormat.class)
public class ImplicantTableFormat extends AbstractFormat {

    public static final String ID = "itnet";

    public ImplicantTableFormat() {
        super(ID, "Implicant Table format", MultivaluedSupport.MULTIVALUED);
    }

    @Override
    public ImplicantTableExport getExporter(LogicalModel model) {
        return new ImplicantTableExport(model);
    }


    @Override
    public ImplicantTableImport getLoader() {
        return new ImplicantTableImport();
    }
}
