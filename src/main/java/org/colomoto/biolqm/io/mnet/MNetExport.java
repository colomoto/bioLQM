package org.colomoto.biolqm.io.mnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

/**
 * Export logical model into a list of multi-valued functions.
 * 
 * @author Aurelien Naldi
 */
public class MNetExport {

	/**
	 * Export a logical model into logical functions.
	 *
	 * @param model the exported model
	 * @param out the output stream
	 * @throws IOException if writing fails
	 */
	public void export(LogicalModel model, OutputStream out) throws IOException {
		MDDManager ddmanager = model.getMDDManager();
		MDDVariable[] variables = ddmanager.getAllVariables();
		PathSearcher searcher = new PathSearcher(ddmanager, true);

		Writer writer = new OutputStreamWriter(out);

		// Collect names and detect the longest one to generate a nice output
		int width = 1;
		int[] functions = model.getLogicalFunctions();
		String[] names = new String[variables.length];
		for (int i=0; i<variables.length; i++) {
			names[i] = variables[i].key.toString();
			int w = names[i].length();
			if (variables[i].nbval > 2) {
				w += 2;
			}
			if (w > width) {
				width = w;
			}
		}

		for (int idx=0; idx<names.length; idx++) {
			int function = functions[idx];
			MDDVariable var = variables[idx];
			int length = width - names[idx].length();
			if (var.nbval > 2) {
				length -= 2;
			}
			String gap = new String(new char[length]).replace("\0", " ");

			// directly write fixed values
			if (ddmanager.isleaf(function)) {
				writer.write(var+gap+" <- " + function+"\n");
				continue;
			}
			
			// write a normalised logical function if the value is not fixed
			int[] path = searcher.setNode(function);
			int[] max = searcher.getMax();
			StringBuffer[] funcBuffers = new StringBuffer[var.nbval-1];
			for (int leaf: searcher) {
				if (leaf == 0) {
					continue;
				}

				StringBuffer sb = funcBuffers[leaf-1];
				if (sb == null) {
					sb = new StringBuffer();
					funcBuffers[leaf-1] = sb;
				} else {
					sb.append(" | ");
				}
				
				// write each constraint
				boolean andFirst = true;
				for (int j=0 ; j<path.length ; j++) {
					int cst_min = path[j];
					int cst_max = max[j];
					if (cst_min < 0) {
						continue;
					}

					MDDVariable reg = variables[j];
					String regname = reg.key.toString();

					if (cst_max + 1 < reg.nbval) {
						cst_max++;
					} else {
						cst_max = -1;
					}

					if (!andFirst) {
						sb.append(" & ");
					}
					andFirst = false;

					if (cst_min == 0 && cst_max == 0) {
						sb.append("!"+regname);
						continue;
					}

					// Build multivalued ranges
					if (cst_min > 0) {
						sb.append(regname);
						if (cst_min > 1) {
							sb.append(":" + cst_min);
						}
					}

					if (cst_max >= cst_min) {
						if (cst_min > 0) {
							sb.append(" & ");
						}
						sb.append("!"+regname);
						if (cst_max > 1) {
							sb.append(":"+(cst_max));
						}
					}
				}
			}

			if (var.nbval > 2) {
				for (int i=0 ; i<funcBuffers.length ; i++) {
					StringBuffer sb = funcBuffers[i];
					if (sb != null) {
						int v = i + 1;
						writer.write(var + ":" + v+ gap  + " <- " + funcBuffers[i].toString() + "\n");
					}
				}
			} else {
				writer.write(var+gap + " <- " + funcBuffers[0].toString() + "\n");
			}
		}
		writer.close();
	}

}
