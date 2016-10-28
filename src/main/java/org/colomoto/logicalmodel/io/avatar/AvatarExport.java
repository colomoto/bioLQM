package org.colomoto.logicalmodel.io.avatar;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import org.colomoto.logicalmodel.LogicalModel;

/**
 * Facilities to export a logical model into an Avatar file  
 * 
 * @author Rui Henriques, Pedro Monteiro
 * @version 1.0
 */
public class AvatarExport {

	private final LogicalModel model;
	
	/**
	 * Instantiates the exportation facilities
	 * @param model the (possibly stateful) logical model
	 */
	public AvatarExport(LogicalModel model) {
		this.model = model;
	}

	/**
	 * Exports the logical model into an AVATAR file
	 * @param out the .AVATAR file to save the logical  model
	 * @throws AvatarLogicalModelException
	 * @throws IOException
	 */
	public void export(File out) throws AvatarLogicalModelException, IOException {
		System.out.println("Exporting Avatar from model to file");
		AvatarWriter.write(model, new FileWriter(out));
		System.out.println("Avatar successfully exported");
	}

	/**
	 * Streams the logical model into a given output recipient 
	 * @param out the output recipient
	 * @throws AvatarLogicalModelException
	 * @throws IOException
	 */
	public void export(OutputStream out) throws AvatarLogicalModelException, IOException {
		AvatarWriter.write(model, new OutputStreamWriter(out));
	}
}
