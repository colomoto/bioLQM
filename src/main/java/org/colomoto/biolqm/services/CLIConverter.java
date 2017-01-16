package org.colomoto.biolqm.services;

import java.io.File;
import java.io.IOException;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.LogicalModelFormat.MultivaluedSupport;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.colomoto.biolqm.modifier.booleanize.Booleanizer;

/**
 * Helper for command-line conversions: define input and output formats
 * and (optional) output folder, then provide files to convert.
 * 
 * @author Aurelien Naldi
 */
public class CLIConverter {

	private static final ServiceManager manager = ServiceManager.getManager();
	
	private final LogicalModelFormat inputFormat;
	private final LogicalModelFormat outputFormat;
	private final File outputFolder;
    private final MultivaluedSupport outputMultivalued;
	
	/**
	 * Create a converter with input and output formats.
	 * 
	 * @param s_in
	 * @param s_out
	 */
	public CLIConverter(String s_in, String s_out) {
		this(s_in, s_out, null);
	}
	
	/**
	 * Create a converter with formats and output directory.
	 * 
	 * @param s_in
	 * @param s_out
	 * @param outDir
	 */
	public CLIConverter(String s_in, String s_out, File outDir) {
		inputFormat = getFormat(s_in);
		outputFormat = getFormat(s_out);

		this.outputMultivalued = outputFormat.getMultivaluedSupport();

		// check that the provided formats exist and have the right capabilities
		if (inputFormat == null) {
			if (outputFormat == null) {
				throw new RuntimeException("Unknown formats: "+s_in+", "+s_out);
			}
			throw new RuntimeException("Unknown format for: "+s_in);
		}
		if (outputFormat == null) {
			throw new RuntimeException("Unknown format for: "+s_out);
		}
		
		if (!inputFormat.canImport()) {
			throw new RuntimeException(inputFormat.getID() +" Format does not support import");
		}
		if (!outputFormat.canExport()) {
			throw new RuntimeException(outputFormat.getID() +" Format does not support export");
		}

		// set output directory
		if (outDir == null) {
			outputFolder = null;
		} else if (!outDir.isDirectory()) {
			throw new RuntimeException("Invalid output folder: "+outDir);
		} else {
			outputFolder = outDir;
		}
	}

	/**
	 * Convert a file to the pre-defined output directory.
	 * 
	 * @param inputFile
	 */
	public void convert(String inputFile) {
		if (outputFolder == null) {
			throw new RuntimeException("Missing output folder");
		}

		// pick an output name
		String outname = inputFile.substring(inputFile.lastIndexOf(File.separator)+1);
		if (outname.contains(".")) {
			outname = outname.substring(0, outname.lastIndexOf("."));	
		}
		outname = outputFolder.getAbsolutePath()+File.separator+ outname + "." + outputFormat.getID();

		//System.out.println("Convert: " + inputFile + " to " + outname);
		convert(inputFile, outname);
	}

	/**
	 * Convert a file providing an output filename explicitly.
	 * 
	 * @param inputFile
	 * @param outputFile
	 */
	public void convert(String inputFile, String outputFile) {

        try {
            LogicalModel model = inputFormat.importFile(new File(inputFile));

            if (!model.isBoolean()) {
                if (outputMultivalued == MultivaluedSupport.BOOLEANIZED) {
                    System.out.println("Warning: Exporting a Booleanized version of the multivalued model");
                    model = Booleanizer.booleanize(model);
                } else if (outputMultivalued == MultivaluedSupport.BOOLEAN_STRICT) {
                    System.err.println("This format does not support multivalued models");
                    return;
                }
            }

            OutputStreamProvider out = new OutputStreamProvider(outputFile);
            outputFormat.export(model, out);
        } catch (IOException e) {
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
	
}
