package org.colomoto.biolqm.io.truthtable;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.implicants.RestrictedPathSearcher;
import org.colomoto.biolqm.io.BaseExporter;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

import java.io.IOException;
import java.io.Writer;
import java.util.List;


/**
 * Export logical model into cnet truth tables.
 *
 * @author Hannes Klarner (minor modifications to a file of Aurelien Naldi)
 */
public class LogicalTruthTableExport extends BaseExporter {

    public LogicalTruthTableExport(LogicalModel model) {
        super(model);
    }

    public void export() throws IOException {
        MDDManager ddmanager = model.getMDDManager();
        MDDVariable[] variables = ddmanager.getAllVariables();
        PathSearcher searcher = new PathSearcher(ddmanager);

        int[] functions = model.getLogicalFunctions();
        List<NodeInfo> components = model.getComponents();

        Writer writer = streams.writer();
        writer.write("# Model stored as a collection of truth tables\n");
        writer.write("# "+components+"\n\n");

        RestrictedPathSearcher implicants = new RestrictedPathSearcher(ddmanager);

        for (int i=0; i<components.size(); i++) {
            // Truth table for component i
            NodeInfo ni = components.get(i);
            byte[] implicant = implicants.setNode(functions[i]);
            int[] regulators = implicants.getRegulatorList();

            // Header line: ID of the component and its regulators
            writer.write(ni.getNodeID());
            int max = ni.getMax();
            if (max > 1) {
                writer.write("["+max+"]");
            }
            writer.write(":");
            for (int j=0; j<regulators.length; j++) {
                writer.write(" ");
                writer.write(components.get(regulators[j]).getNodeID());
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
                row[k++] = (char)('0'+v);
                row[k++] = ':';
                for ( int value: implicant) {
                    // -1 stands for don't care
                    if (value < 0) {
                        row[k++] = '-';
                    } else {
                        row[k++] = (char)('0'+value);
                    }
                }
                row[k++] = '\n';
                writer.write(row);
            }
            writer.write("\n");
        }

        writer.close();
    }

}
