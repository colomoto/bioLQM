package org.colomoto.biolqm.io.pint;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseExporter;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;

/**
 * Export logical model into the Pint format.
 *
 * @author Loic Pauleve
 */
public class PintExport extends BaseExporter {

	public PintExport(LogicalModel model, StreamProvider streams) {
		super(model, streams);
	}

	@Override
	public void export() throws IOException {

		final List<NodeInfo> nodes = model.getComponents();

		Writer writer = streams.writer();

		writer.write("(* This model has been automatically generated using colomoto/logicalmodels\n");
		writer.write(" * You may want to optimize this model for pint using the following command:\n");
		writer.write("       pint-export --simplify -i model.an -o model.an\n");
		writer.write(" * where model.an is this file.\n *)\n\n");

		/*
		 * Automata declaration
		 */
		int i;
		for (NodeInfo ni: nodes) {
			writer.write('"'+ni.getNodeID()+"\" [0");
			for (i = 1; i <= ni.getMax(); ++i) {
				writer.write(", "+i);
			}
			writer.write("]\n");
		}
		writer.write("\n");

		/*
		 * Transitions
		 */
		MDDManager ddmanager = model.getMDDManager();
		PathSearcher searcher = new PathSearcher(ddmanager);

		int[] functions = model.getLogicalFunctions();
		for (int idx=0 ; idx<functions.length ; idx++) {
			NodeInfo ni = nodes.get(idx);

			// write a normalised logical function
			int[] path = searcher.setNode(functions[idx]);
			for (int leaf: searcher) {
				int selfcond = -1;

				boolean first = true;
				String cond = "";
				for (i=0 ; i<path.length ; i++) {
					int cst = path[i];
					if (cst < 0) {
						continue;
					}
					if (i == idx) {
						selfcond = cst;
						continue;
					}
					if (!first) {
						cond += " and ";
					}
					first = false;
					cond += '"'+nodes.get(i).toString()+"\"="+cst;
				}

				int omin = 0;
				int omax = ni.getMax();
				if (selfcond >= 0) {
					omin = omax = selfcond;
				}
				for (i = omin; i <= omax; ++i) {
					if (leaf == i) {
						continue;
					}
					int j = i < leaf ? i+1 : i-1;
					writer.write('"'+ni.toString()+"\" " + i + " -> " + j);
					if (!first) {
						writer.write(" when " + cond);
					}
					writer.write("\n");
				}
			}
			writer.write("\n");
		}

		/* TODO: initial state? */

		writer.close();
	}
}

