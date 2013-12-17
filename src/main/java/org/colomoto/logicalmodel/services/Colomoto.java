package org.colomoto.logicalmodel.services;

import java.io.File;

import org.colomoto.logicalmodel.io.FormatMultiplexer;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.colomoto.logicalmodel.tool.LogicalModelTool;

/**
 * Simple command-line interface for the CoLoMoTo toolbox.
 * For now, all it does is format conversion.
 * 
 * @author Aurelien Naldi
 */
public class Colomoto {

	private static final String FORMAT_SEPARATOR = ":";
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		if (args.length < 2) {
			help();
		} else if ("-r".equals(args[0]) ) {
			String toolID = args[1].trim();
			CLIToolRunner runner = new CLIToolRunner(args[1].trim());
			
			LogicalModelFormat format = null;
			int curArg = 2;
			if ("-f".equals(args[curArg]) ) {
				runner.setFormat(args[curArg+1]);
				curArg += 2;
			}

			for (int i=curArg ; i<args.length ; i++) {
				runner.run(args[i].trim());
			}
		} else if (args.length == 2) {
			CLIConverter cli = new CLIConverter(args[0], args[1]);
			cli.convert(args[0], args[1]);
			
		} else {
			String s_convert = args[0];
			String[] formats = s_convert.split(FORMAT_SEPARATOR);
			if (formats == null || formats.length != 2) {
				throw new RuntimeException("Invalid format conversion flag: "+s_convert);
			}
			
			File outDir = new File(args[args.length-1]);
			if (!outDir.isDirectory()) {
				outDir = null;
				
				if (args.length > 3) {
					throw new RuntimeException("Multiple conversion require an existing output directory");
				}
				
				CLIConverter cli = new CLIConverter(formats[0], formats[1]);
				cli.convert(args[1], args[2]);
			} else {			
				CLIConverter cli = new CLIConverter(formats[0], formats[1], outDir);
				for (int i=1 ; i<args.length-1 ; i++) {
					cli.convert(args[i]);
				}
			}
		}
	}

	public static void error(String message) {
		System.err.println(message);
		help();
	}
	
	public static void help() {
		String separator = "------------------------------------------------------------------------------------\n";
		String command = "java -jar LogicalModel.jar";
		StringBuffer sb = new StringBuffer();
		sb.append(separator+"| Usage: \n"+separator);
		sb.append("# Convert a single file:\n");
		sb.append(command).append(" [informat");
		sb.append(FORMAT_SEPARATOR).append("outformat] infile outfile\n");

		sb.append("\n# Convert a group of files:\n");
		sb.append(command).append(" informat");
		sb.append(FORMAT_SEPARATOR).append("outformat infile1...infilen outfolder\n");
		
		sb.append("\n# Run a tool on an imported model:\n");
		sb.append(command).append(" -r tool infile\n");

		
		sb.append("\n\n"+separator+"| Available formats: (use @ to select subformats)\n");
		sb.append("|   '<'/'>': import / export  ; 'B'/'M' Boolean/Multivalued\n");
		sb.append(separator);
		for (LogicalModelFormat format: ServiceManager.getManager().getFormats()) {
			String cap;
			if (format.canImport()) {
				if (format.canExport()) {
					cap = "  <>";
				} else {
					cap = "  < ";
				}
			} else {
				if (format.canExport()) {
					cap = "   >";
				} else {
					cap = "  --";
				}
			}
			
			String level = format.supportsMultivalued() ? "M " : "B ";

			String extra = "";
			if (format instanceof FormatMultiplexer) {
				Enum[] subformats = ((FormatMultiplexer)format).getSubformats();
				extra += "@[";
				boolean first = true;
				for (Enum e: subformats) {
					if (first) {
						first = false;
					} else  {
						extra += ",";
					}
					extra += e;
				}
				extra += "]";
			}
			sb.append(level).append(cap).append(" ").append(format.getID()).append(extra);
			sb.append("\t").append(format.getName()).append("\n");
		}
		
		
		sb.append("\n\n"+separator+"| Available tools:\n"+separator);
		for (LogicalModelTool tool: ServiceManager.getManager().getTools()) {
			String level = tool.supportsMultivalued() ? "M " : "B ";
			sb.append(level).append(" ").append(tool.getID());
			sb.append("\t").append(tool.getName()).append("\n");
		}
		
		
		sb.append("\n\n"+separator+"| Examples:\n"+separator);
		sb.append(command).append(" infile.sbml outfile.ginml\n");
		sb.append(command).append(" sbml").append(FORMAT_SEPARATOR);
		sb.append("PN@INA infile.xml file.txt\n");
		sb.append(command).append(" sbml").append(FORMAT_SEPARATOR);
		sb.append("ginml file1.in...filen.in /path/to/outfolder/\n");
		sb.append(command).append(" -r stable infile.sbml\n");
		
		System.out.println(sb);
	}

}
