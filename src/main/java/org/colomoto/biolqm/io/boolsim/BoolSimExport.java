package org.colomoto.biolqm.io.boolsim;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.BaseExporter;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

import java.io.IOException;
import java.io.Writer;

/**
 * Export logical model into the boolsim format.
 * 
 * @author Aurelien Naldi
 * @author Julien Dorier
 */
public class BoolSimExport extends BaseExporter {


	public BoolSimExport(LogicalModel model) {
		super(model);
	}

	@Override
	public void export() throws IOException {
		MDDManager ddmanager = model.getMDDManager();
		MDDVariable[] variables = ddmanager.getAllVariables();
		PathSearcher searcher = new PathSearcher(ddmanager);

		Writer writer = streams.writer();
		int[] functions = model.getLogicalFunctions();
		for (int idx=0 ; idx<functions.length ; idx++) {
			MDDVariable var = variables[idx];

			// write a normalised logical function
			int[] path = searcher.setNode(functions[idx]);
			for (int leaf: searcher) {
				if (leaf == 0) {
					continue;
				}

				// write each constraint
				boolean first = true;
				boolean fixed = true;
				for (int i=0 ; i<path.length ; i++) {
					int cst = path[i];
					if (cst < 0) {
						continue;
					}
					
					fixed = false;
					if (!first) {
						writer.write("&");
					}

					if (cst == 0) {
						writer.write("^"+variables[i].key);
					} else {
						// FIXME: adapt for multivalued
						writer.write(variables[i].key.toString());
					}
					first = false;
				}
				if (fixed) {
					// write a "always true" function
					writer.write(var+" -> "+var+"\n");
					writer.write("^"+var+" -> "+var+"\n");
				} else {
					writer.write(" -> "+var+"\n");
				}
			}
		}

		writer.close();
	}
}
