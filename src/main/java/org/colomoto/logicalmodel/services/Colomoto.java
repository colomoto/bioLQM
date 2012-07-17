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

	private static final ServiceManager manager = ServiceManager.getManager();
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		Colomoto cli = new Colomoto();
		
		if (args.length == 2) {
			cli.convert(args[0], args[1]);
		} else {
			cli.help();
		}
	}

	private void convert(String inputFile, String outputFile) {
		
		// guess formats from filenames
		LogicalModelFormat inputFormat = guessFormat(inputFile);
		LogicalModelFormat outputFormat = guessFormat(outputFile);
		
		if (inputFormat == null || outputFormat == null) {
			help();
			return;
		}
		
		if (!inputFormat.canImport()) {
			error("Input format does not support import: "+inputFormat);
			return;
		}
		if (!outputFormat.canExport()) {
			error("Output format does not support export: "+outputFormat);
			return;
		}
		
		// actual conversion
		try {
			LogicalModel model = inputFormat.importFile(new File(inputFile));
			OutputStream out = new FileOutputStream(outputFile);
			outputFormat.export(model, out);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private LogicalModelFormat guessFormat(String filename) {
		
		String extension = filename.substring(filename.lastIndexOf('.')+1);
		
		System.out.println(extension);
		
		return manager.getFormat(extension);
	}
	
	public void error(String message) {
		System.err.println(message);
		help();
	}
	
	public void help() {
		System.out.println("Supported formats");
		System.out.println("-----------------");
		for (LogicalModelFormat format: manager.getFormats()) {
			System.out.println(format);
		}
		System.out.println();

	}

}
