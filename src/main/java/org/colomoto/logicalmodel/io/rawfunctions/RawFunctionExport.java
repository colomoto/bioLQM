package org.colomoto.logicalmodel.io.rawfunctions;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

/**
 * Export logical model into a raw list of functions.
 * 
 * @author Aurelien Naldi
 */
public class RawFunctionExport {

	/**
	 * Export a logical model into logical functions.
	 * 
	 * @param model
	 * @param out
	 * @throws IOException
	 */
	public void export(LogicalModel model, OutputStream out) throws IOException {
		MDDManager ddmanager = model.getMDDManager();
		MDDVariable[] variables = ddmanager.getAllVariables();
		PathSearcher searcher = new PathSearcher(ddmanager);
		
		Writer writer = new OutputStreamWriter(out);
		int[] functions = model.getLogicalFunctions();
		for (int idx=0 ; idx<functions.length ; idx++) {
			MDDVariable var = variables[idx];
			writer.write(var+": ");
			
			int function = functions[idx];
			
			// directly write fixed values
			if (ddmanager.isleaf(function)) {
				writer.write(function+"\n");
				continue;
			}
			
			// write a normalised logical function if the value is not fixed
			int[] path = searcher.setNode(function);
			boolean first = true;
			for (int leaf: searcher) {
				if (leaf == 0) {
					continue;
				}
				
				if (!first) {
					writer.write(" | ");
				}
				
				writer.write("(");
				
				// write each constraint
				first = true;
				for (int i=0 ; i<path.length ; i++) {
					int cst = path[i];
					if (cst < 0) {
						continue;
					}
					
					if (!first) {
						writer.write(" & ");
					}
					
					if (cst == 0) {
						writer.write("!"+variables[i].key);
					} else {
						// FIXME: adapt for multivalued
						writer.write(variables[i].key.toString());
					}
					first = false;
				}
				writer.write(")");
				first = false;
			}
			writer.write("\n");
		}
		
		writer.close();
	}

}
