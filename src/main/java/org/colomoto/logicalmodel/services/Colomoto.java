package org.colomoto.logicalmodel.services;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;
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
		System.out.println("Format conversion");
		System.out.println("===================");
		System.out.println("  <file.in> <file.out>: import file.in and export it to file.out (guess formats from file extensions)");
		System.out.println("  in"+FORMAT_SEPARATOR+"out <file.in> <file.out>: import file.in and export it to file.out (provide formats explicitely)");
		System.out.println("  in"+FORMAT_SEPARATOR+"out <file_1.in> [ <file_[2..n].in> ] <folder.out>: import a list of files and export them to the output folder folder.out (provide formats explicitely)");
		System.out.println();
		System.out.println("Supported formats");
		System.out.println("-----------------");
		for (LogicalModelFormat format: ServiceManager.getManager().getFormats()) {
			System.out.println(format);
		}
		System.out.println();
		System.out.println("Examples");
		System.out.println("-----------------");
		System.out.println("TODO");
	}

}
