package org.colomoto.biolqm.io.cnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.helper.implicants.RestrictedPathSearcher;
import org.colomoto.biolqm.io.BaseExporter;
import org.colomoto.biolqm.io.StreamProvider;
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
public class CNetExport extends BaseExporter {

    public CNetExport(LogicalModel model) {
        super(model);
    }

    public void export() throws IOException {
        MDDManager ddmanager = model.getMDDManager();
        MDDVariable[] variables = ddmanager.getAllVariables();
        PathSearcher searcher = new PathSearcher(ddmanager);

        Writer writer = streams.writer();
        writer.write("# model in CNET format, used by the tool BNS of E. Dubrova\n");
        writer.write("# see https://people.kth.se/~dubrova/bns.html\n");
        writer.write("\n");

        int[] functions = model.getLogicalFunctions();

        List<NodeInfo> components = model.getComponents();

        writer.write(".v "+components.size()+"\n");
        writer.write("# component order:\n");
        writer.write("# "+components+"\n");
        writer.write("\n");

        RestrictedPathSearcher implicants = new RestrictedPathSearcher(ddmanager);

        for (int i=0; i<components.size(); i++) {
            writer.write("# "+components.get(i).toString()+"\n");

            // select all paths in dd that are true


            byte[] implicant = implicants.setNode(functions[i]);
            int[] regulators = implicants.getRegulatorList();


            String indices = "";
            for (int j=0; j<regulators.length; j++) {
                indices = indices + " " + (regulators[j]+1);
            }
            writer.write(".n "+(i+1)+" "+regulators.length+indices+"\n");


            // implicant = [1,1,0,-1] contains 1,0 and -1 for dont cares

            for (int v: implicants) {

                String row = "";
                for (int k=0; k<implicant.length; k++) {
                    if (implicant[k]==-1) {
                        row = row + "-";
                    } else {
                        row = row +implicant[k];
                    }
                }
                if (row.length()>0) {row = row + " ";}
                writer.write(row+v+"\n");
            }
            writer.write("\n");
        }

        writer.close();
    }

}
