package org.colomoto.biolqm.services;

import java.io.IOException;
import java.util.Arrays;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.tool.LogicalModelTool;

import javax.script.ScriptEngine;

/**
 * Simple command-line interface for the CoLoMoTo toolbox.
 * For now, all it does is format conversion.
 * 
 * @author Aurelien Naldi
 */
public class Colomoto {

	private static final ServiceManager manager = ServiceManager.getManager();

	/**
	 * @param args
	 */
	public static void main(String[] args) {

        ExtensionLoader.loadExtensions("extensions", Colomoto.class);

		if (args.length < 2) {
			error("not enough arguments");
			return;
        }
		
		if ("-s".equals(args[0]) ) {
			runScript(args, true);
            return;
        }
		
		// no-compat script: temporary addition to facilitate the transition to LQM API
		if ("-ncs".equals(args[0]) ) {
			runScript(args, false);
            return;
        }
		
		int argIdx = 0;
		String inputFormat = null;
		
		if ("-if".equals(args[argIdx])) {
			if (args.length < argIdx+4) {
				error("Not enough arguments after input format");
				return;
			}
			argIdx++;
			inputFormat = args[argIdx++];
		}
		
		String inputFilename = args[argIdx++];
		LogicalModel model = ScriptLauncher.loadModel(inputFilename, inputFormat);
		
		if ("-m".equals(args[argIdx])) {
			if (args.length < argIdx+3) {
				error("Not enough arguments after modifier");
				return;
			}
			argIdx++;
			String s_modifier = args[argIdx++];
			// TODO: handle modifier parameters
			String modifierName = s_modifier;
			ModelModifierService modifier = ServiceManager.getManager().getModifier(modifierName);
			model = modifier.getModifiedModel(model, "");
		}

		if ("-r".equals(args[argIdx]) ) {
			if (args.length < argIdx+2) {
				error("Not enough arguments after runnable tool");
				return;
			}
			argIdx++;
			String toolID = args[argIdx++];
			LogicalModelTool tool = ServiceManager.getManager().getTool(toolID);
			if (tool == null) {
				throw new RuntimeException("Unknown tool: "+toolID);
			}
			try {
				tool.run(model);
			} catch (IOException e) {
				e.printStackTrace();
			}
			return;
		}

		String outputFormat = null;
		if ("-of".equals(args[argIdx])) {
			if (args.length < argIdx+3) {
				error("Not enough arguments after output format");
				return;
			}
			argIdx++;
			outputFormat = args[argIdx++];
		}
		
		String outputTarget = args[argIdx++];
		ScriptLauncher.saveModel(model, outputTarget, outputFormat);

		if (argIdx < args.length) {
			error((args.length-argIdx) + " remaining arguments "+args.length + "  "+ argIdx);
		}
	}

	private static void runScript(String[] args, boolean compatible_mode) {

        String scriptname = args[1];
        ScriptEngine engine = null;
        try {
            engine = ScriptEngineLoader.loadEngine(scriptname);
        } catch (Exception e) {
            System.out.println(e.getMessage());
            return;
        }
        
        // copy relevant arguments
        String[] scriptargs = Arrays.copyOfRange(args, 2, args.length);

        try {
            // add the launcher variable and actually run the script
			ScriptLauncher lqm = new ScriptLauncher(scriptargs);
			engine.put("lqm", lqm);

			if (compatible_mode) {
				engine.put("lm", lqm);
				engine.put("args", scriptargs);
			}
            engine.eval(new java.io.FileReader(scriptname));
        } catch (Exception e) {
            e.printStackTrace();
        }
		
	}
	
	public static LogicalModelFormat getFormat(String name) {

		// dealing with a "normal" format identification
		LogicalModelFormat format = manager.getFormat(name);
		if (format == null) {
			// try using a file extension
			String extension = name.substring(name.lastIndexOf('.')+1);
			format = manager.getFormat(extension);
		}
		
		return format;
	}
	
	public static void error(String message) {
		System.err.println(message);
		help();
	}
	
	public static void help() {
		String separator = "------------------------------------------------------------------------------------\n";
		String command = "java -jar bioLQM.jar";
		StringBuffer sb = new StringBuffer();
		sb.append(separator+"| Usage: \n"+separator);
		sb.append("# Convert a single file:\n");
		sb.append(command).append(" [-if informat] infile [-m modifier] [-of outformat] outfile\n");

        sb.append("\n# Run a tool on an imported model:\n");
        sb.append(command).append(" [-if informat] infile [-m modifier] -r tool\n");

        sb.append("\n# Run a script:\n");
        sb.append(command).append(" -s script.js [arguments...]\n");

        sb.append("\n\n"+separator+"| Available formats:\n");
		sb.append("|   '<'/'>': import / export  ; 'b'/'B'/'M' Boolean/Booleanized/Multivalued\n");
		sb.append(separator);

		// detect the longest format name to generate a nice output
		int namelength = 10;
		for (LogicalModelFormat format: ServiceManager.getManager().getFormats()) {
			String id = format.getID();
			if (id.length() > namelength) {
				namelength = id.length();
			}
		}

		String nameformat = "%1$-"+namelength+"s    ";
		for (LogicalModelFormat format: ServiceManager.getManager().getFormats()) {
			String cap;
			if (format.canImport()) {
				if (format.canExport()) {
					cap = " <> ";
				} else {
					cap = " <  ";
				}
			} else {
				if (format.canExport()) {
					cap = "  > ";
				} else {
					cap = " -- ";
				}
			}
			
			String level = format.getMultivaluedSupport().flag;
			String id = String.format(nameformat, format.getID());
			sb.append(level).append(cap).append(" ").append(id);
			sb.append("\t").append(format.getName()).append("\n");
		}


		// detect the longest modifier ID and name to generate a nice output
		int idlength = 5;
		namelength = 10;
		for (ModelModifierService modifier: ServiceManager.getManager().getModifiers()) {
			String id = modifier.getID();
			String name = modifier.getName();
			if (id.length() > idlength) {
				idlength = id.length();
			}
			if (name.length() > namelength) {
				namelength = name.length();
			}
		}

		String idformat = "%1$-"+idlength+"s    ";
		nameformat = "%1$-"+namelength+"s    ";
		sb.append("\n\n"+separator+"| Available modifiers:\n"+separator);
		for (ModelModifierService modifier: ServiceManager.getManager().getModifiers()) {
			sb.append(String.format(idformat, modifier.getID()) + "\t" + String.format(nameformat, modifier.getName()) + modifier.getDescription() + "\n");
		}

		sb.append("\n\n"+separator+"| Available tools:\n"+separator);
		for (LogicalModelTool tool: ServiceManager.getManager().getTools()) {
			String level = tool.supportsMultivalued() ? "M " : "B ";
			sb.append(level).append(" ").append(tool.getID());
			sb.append("\t").append(tool.getName()).append("\n");
		}
		
		
		sb.append("\n\n"+separator+"| Examples:\n"+separator);
		sb.append(command).append(" infile.sbml outfile.ginml\n");
		sb.append(command).append(" infile.sbml -m booleanize outfile.sbml\n");
		sb.append(command).append(" -if sbml infile.xml -of ina file.txt\n");
		sb.append(command).append(" -if sbml file1.in...filen.in -of ginml /path/to/outfolder/\n");
		sb.append(command).append(" infile.sbml -r stable\n");
		
		System.out.println(sb);
	}

}
