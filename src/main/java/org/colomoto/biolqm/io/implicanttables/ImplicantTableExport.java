package org.colomoto.biolqm.io.implicanttables;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.implicants.RestrictedPathSearcher;
import org.colomoto.biolqm.io.BaseExporter;
import org.colomoto.mddlib.MDDManager;

import java.io.IOException;
import java.io.Writer;
import java.util.List;


/**
 * Export logical model into lists of implicant tables.
 *
 * @author Hannes Klarner
 * @author Aurelien Naldi
 */
public class ImplicantTableExport extends BaseExporter {

    public ImplicantTableExport(LogicalModel model) {
        super(model);
    }

    public void export() throws IOException {
        MDDManager ddmanager = model.getMDDManager();
        int[] functions = model.getLogicalFunctions();
        List<NodeInfo> components = model.getComponents();

        Writer writer = streams.writer();
        writer.write("# Logical model defined as a collection of implicant tables\n");
        writer.write("# "+components+"\n\n");

        RestrictedPathSearcher implicants = new RestrictedPathSearcher(ddmanager);

        for (int i=0; i<components.size(); i++) {
            // Implicant table for component i
            NodeInfo ni = components.get(i);
            byte[] implicant = implicants.setNode(functions[i]);
            int[] regulators = implicants.getRegulatorList();

            // Header line: the list of regulators and the target component
            for (int j=0; j<regulators.length; j++) {
                writer.write(components.get(regulators[j]).getNodeID());
                writer.write(" ");
            }
            // Add a separator before the target component
            writer.write(": ");
            writer.write(ni.getNodeID());
            if (ni.getMax() > 1) {
                writer.write("["+ni.getMax()+"]");
            }
            writer.write("\n");

            // Write all true paths from the dd
            int len = regulators.length + 3;
            for (int v: implicants) {
                if (v == 0) {
                    continue;
                }
                char[] row = new char[len];
                int k=0;
                for ( int value: implicant) {
                    // -1 stands for don't care
                    if (value < 0) {
                        row[k++] = '-';
                    } else {
                        row[k++] = (char)('0'+value);
                    }
                }
                row[k++] = ':';
                row[k++] = (char)('0'+v);
                row[k++] = '\n';
                writer.write(row);
            }
            writer.write("\n");
        }

        writer.close();
    }

}
