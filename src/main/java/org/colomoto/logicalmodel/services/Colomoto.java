package org.colomoto.logicalmodel.services;

import java.io.File;

import org.colomoto.logicalmodel.io.FormatMultiplexer;
import org.colomoto.logicalmodel.io.LogicalModelFormat;

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
		String command = "java -jar LogicalModel.jar";
		StringBuffer sb = new StringBuffer();
		sb.append("Usage: ").append(command).append(" [informat");
		sb.append(FORMAT_SEPARATOR).append("outformat] infile outfile\n");
		sb.append("  for single file format conversion, where the ");
		sb.append("import/export file formats \n  are inferred from ");
		sb.append("file extensions, if not explicitly specified.\n");
		sb.append("\n");
		sb.append("Usage: ").append(command).append(" informat");
		sb.append(FORMAT_SEPARATOR).append("outformat infile1...infilen outfolder\n");
		sb.append("  for batch file format conversion, where the ");
		sb.append("import/export file formats are mandatory.\n");
		sb.append("\n");
		sb.append("List of supported formats:\n");
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
			sb.append(cap).append(" ").append(format.getID()).append(extra);
			sb.append("\t").append(format.getName()).append("\n");
		}
		sb.append("where:\n");
		sb.append("  '<' and '>' indicate import and export support capabilities\n");
		sb.append("  '@' indicates the available subformats\n");
		sb.append("\n");
		sb.append("Examples:\n");
		sb.append("  ").append(command).append(" infile.sbml outfile.ginml\n");
		sb.append("  ").append(command).append(" sbml").append(FORMAT_SEPARATOR);
		sb.append("PN@INA infile.xml file.txt\n");
		sb.append("  ").append(command).append(" sbml").append(FORMAT_SEPARATOR);
		sb.append("ginml file1.in...filen.in /path/to/outfolder/");
		System.out.println(sb);
	}

}
