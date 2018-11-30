package org.colomoto.biolqm.io.implicanttables;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.implicants.RestrictedPathSearcher;
import org.colomoto.biolqm.helper.state.PatternStateIterator;
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

    private boolean expand = true;

    public ImplicantTableExport(LogicalModel model) {
        super(model);
    }

    public void expand(boolean expand) {
        this.expand = expand;
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
            byte[] maxs = new byte[regulators.length];

            // Header line: the list of regulators and the target component
            for (int j=0; j<regulators.length; j++) {
                maxs[j] = components.get(regulators[j]).getMax();
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
            PatternStateIterator expander = null;
            if (expand) {
                expander = new PatternStateIterator(maxs, maxs);
            }
            for (int v: implicants) {
                if (v == 0) {
                    continue;
                }
                if (expander == null) {
                    writeState(writer, implicant, v);
                    continue;
                }

                // expand each implicant
                expander.reset(implicant);
                while (expander.hasNext()) {
                    writeState(writer, expander.next(), v);
                }
            }
            writer.write("\n");

        }

        writer.close();
    }

    private void writeState(Writer writer, byte[] state, int target) throws IOException {
        char[] row = new char[state.length + 3];
        int k = 0;
        for (int value : state) {
            // -1 stands for don't care
            if (value < 0) {
                row[k++] = '-';
            } else {
                row[k++] = (char) ('0' + value);
            }
        }
        row[k++] = ':';
        row[k++] = (char) ('0' + target);
        row[k++] = '\n';
        writer.write(row);
    }
}
