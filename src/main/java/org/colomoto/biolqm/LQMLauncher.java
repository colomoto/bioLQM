package org.colomoto.biolqm;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.modifier.booleanize.ModelBooleanizerService;
import org.colomoto.biolqm.modifier.perturbation.PerturbationService;
import org.colomoto.biolqm.tool.LogicalModelTool;

import javax.script.ScriptEngine;

/**
 * Entry point to launch the bioLQM toolbox.
 * It dispatches arguments to the CLI or script mode
 * 
 * @author Aurelien Naldi
 */
public class LQMLauncher {

	/**
	 * The main entry point.
	 * 
	 * @param args the command line arguments
	 */
	public static void main(String[] args) {

        ExtensionLoader.loadExtensions("extensions", LQMLauncher.class);

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
		LogicalModel model = load(inputFilename, inputFormat);
		
		while (true) {
			if ("-m".equals(args[argIdx])) {
				if (args.length < argIdx+3) {
					error("Not enough arguments after modifier");
					return;
				}
				argIdx++;
				String s_modifier = args[argIdx++];
				String s_parameters = "";
				int idx = s_modifier.indexOf(":");
				if (idx >= 0) {
					// Handle modifier parameters
					s_parameters = s_modifier.substring(idx+1);
					s_modifier = s_modifier.substring(0, idx);
				}
				String modifierName = s_modifier;
				ModelModifierService modifier = LQMServiceManager.getModifier(modifierName);
				model = modifier.getModifiedModel(model, s_parameters);
				continue;
			}
			
			if ("-p".equals(args[argIdx])) {
				if (args.length < argIdx+3) {
					error("Not enough arguments after modifier");
					return;
				}
				argIdx++;
				String s_parameters = args[argIdx++];
				ModelModifierService modifier = LQMServiceManager.getModifier(PerturbationService.class);
				model = modifier.getModifiedModel(model, s_parameters);
				continue;
			}
			
			break;
		}
		
		if ("-r".equals(args[argIdx]) ) {
			if (args.length < argIdx+2) {
				error("Not enough arguments after runnable tool");
				return;
			}
			argIdx++;
			String toolID = args[argIdx++];
			LogicalModelTool tool = LQMServiceManager.getTool(toolID);
			if (tool == null) {
				throw new RuntimeException("Unknown tool: "+toolID);
			}
			String[] parameters = new String[args.length - argIdx];
			System.arraycopy(args, argIdx, parameters, 0, parameters.length);
			tool.run(model, parameters);
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
		save(model, outputTarget, outputFormat);

		if (argIdx < args.length) {
			error((args.length-argIdx) + " remaining arguments "+args.length + "  "+ argIdx);
		}
	}

	private static void runScript(String[] args, boolean compatible_mode) {

        String scriptname = args[1];
        ScriptEngine engine = null;
        try {
            engine = LQMScriptLauncher.loadEngine(scriptname);
        } catch (Exception e) {
            System.out.println(e.getMessage());
            return;
        }
        
        // copy relevant arguments
        String[] scriptargs = Arrays.copyOfRange(args, 2, args.length);

        try {
            // add the launcher variable and actually run the script
			LQMScriptLauncher lqm = new LQMScriptLauncher(scriptargs);
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
		LogicalModelFormat format = LQMServiceManager.getFormat(name);
		if (format == null) {
			// try using a file extension
			String extension = name.substring(name.lastIndexOf('.')+1);
			format = LQMServiceManager.getFormat(extension);
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
		for (LogicalModelFormat format: LQMServiceManager.getFormats()) {
			String id = format.getID();
			if (id.length() > namelength) {
				namelength = id.length();
			}
		}

		String nameformat = "%1$-"+namelength+"s    ";
		for (LogicalModelFormat format: LQMServiceManager.getFormats()) {
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
		for (ModelModifierService modifier: LQMServiceManager.getModifiers()) {
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
		for (ModelModifierService modifier: LQMServiceManager.getModifiers()) {
			sb.append(String.format(idformat, modifier.getID()) + "\t" + String.format(nameformat, modifier.getName()) + modifier.getDescription() + "\n");
		}

		sb.append("\n\n"+separator+"| Available tools:\n"+separator);
		for (LogicalModelTool tool: LQMServiceManager.getTools()) {
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

	/**
	 * Load a model from file.
	 * 
	 * @param filename the path to the loaded file
	 * @param format the name of the import format
	 * @return the loaded model
	 */
    public static LogicalModel load(String filename, String format) {
        if (format == null) {
            format = filename.substring(filename.lastIndexOf(".")+1);
        }
        LogicalModelFormat inputFormat = LQMLauncher.getFormat(format);
        if (inputFormat == null) {
            System.err.println("Format not found: " + format);
            return null;
        }

        if (!inputFormat.canImport()) {
            throw new RuntimeException(inputFormat.getID() +" Format does not support import");
        }

        try {
            LogicalModel model = inputFormat.importFile(new File(filename));
            return model;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Save a model to file
     * 
     * @param model the model to save
     * @param filename the path of the output file
     * @param format the name of the export format
     * @return true if success
     */
    public static boolean save(LogicalModel model, String filename, String format) {
        if (format == null) {
            format = filename.substring(filename.lastIndexOf(".")+1);
        }
        LogicalModelFormat outputFormat = LQMLauncher.getFormat(format);
        if (outputFormat == null) {
            System.err.println("Format not found: "+format);
            return false;
        }

        if (!outputFormat.canExport()) {
            throw new RuntimeException(outputFormat.getID() +" Format does not support export");
        }

        try {
			if (!model.isBoolean()) {
	        	switch (outputFormat.getMultivaluedSupport()) {
				case BOOLEAN_STRICT:
		            throw new RuntimeException(outputFormat.getID() +" does not support multivalued models");
				case BOOLEANIZED:
		            System.out.println(outputFormat.getID() +": export of a booleanized model");
		            model = LQMServiceManager.getModifier(ModelBooleanizerService.class).getModifiedModel(model);
					break;
				}
			}
            outputFormat.export(model, new OutputStreamProvider( filename));
            return true;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;

    }

}
