package org.colomoto.biolqm.io.bnet;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

// for pretty exports
import java.util.Arrays;


/**
 * Export logical model into a raw list of functions.
 * 
 * @author Hannes Klarner (minor modifications to a file of Aurelien Naldi)
 */
public class BNetExport {

	/**
	 * Export a logical model into logical functions.
	 *
	 * @param model the exported model
	 * @param out the output stream
	 * @throws java.io.IOException if writing fails
	 */
	public static void export(LogicalModel model, OutputStream out) throws IOException {
		MDDManager ddmanager = model.getMDDManager();
		MDDVariable[] variables = ddmanager.getAllVariables();
		PathSearcher searcher = new PathSearcher(ddmanager);
		

		Writer writer = new OutputStreamWriter(out);
      writer.write("# model in BoolNet format\n");
      writer.write("# the header targets, factors is mandatory to be importable in the R package BoolNet\n");
      writer.write("\n");
      writer.write("targets, factors\n");
            
		int[] functions = model.getLogicalFunctions();
		
      String[] names = new String[variables.length];
      
      for (int i=0; i<variables.length; i++) {
         names[i] = variables[i].key.toString();
      }
      
      
      // detect the longest format name to generate a nice output
		int width = 5;
		for (int i=0; i<names.length; i++) {
			if (names[i].length() > width) {
				width = names[i].length();
			}
		}
      
      String[] names_sorted = Arrays.copyOf(names, names.length);
      Arrays.sort(names_sorted);
		
		for (int i=0; i<names_sorted.length; i++) {
		
		   int idx = Arrays.asList(names).indexOf(names_sorted[i]);
		   
			MDDVariable var = variables[idx];
			int length = width - names[idx].length();
			String gap = new String(new char[length]).replace("\0", " ");
			writer.write(var+", "+gap);
			
			int function = functions[idx];
			
			// directly write fixed values
			if (ddmanager.isleaf(function)) {
				writer.write(function+"\n");
				continue;
			}
			
			// write a normalised logical function if the value is not fixed
			int[] path = searcher.setNode(function);
			boolean first = true;
			boolean multiple = false;
			StringBuffer funcBuffer = new StringBuffer();
			for (int leaf: searcher) {
				if (leaf == 0) {
					continue;
				}
				
				if (!first) {
					funcBuffer.append(" | ");
					multiple = true;
				} else {
					first = false;
				}
				
				// write each constraint
				boolean andFirst = true;
				for (int j=0 ; j<path.length ; j++) {
					int cst = path[j];
					if (cst < 0) {
						continue;
					}
					
					if (!andFirst) {
						funcBuffer.append("&");
					}
					
					if (cst == 0) {
						funcBuffer.append("!"+variables[j].key);
					} else {
						// FIXME: adapt for multivalued
						funcBuffer.append(variables[j].key.toString());
					}
					andFirst = false;
				}
			}
			if (multiple) {
				writer.write(funcBuffer.toString());
			} else {
				writer.write(funcBuffer.toString());
			}
			writer.write("\n");
		}
		
		writer.close();
	}

}
