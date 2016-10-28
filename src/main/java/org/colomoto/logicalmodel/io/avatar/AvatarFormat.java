package org.colomoto.logicalmodel.io.avatar;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.AbstractFormat;
import org.colomoto.logicalmodel.io.LogicalModelFormat;
import org.mangosdk.spi.ProviderFor;

/**
 * Format description for Avatar files
 * 
 * @author Rui Henriques, Pedro Monteiro
 * @version 1.0
 */
@ProviderFor(LogicalModelFormat.class)
public class AvatarFormat extends AbstractFormat {

	/**
	 * Creates a dedicated Avatar format with multi-valued logic
	 */
	public AvatarFormat() {
		super("avatar", "Avatar v1.0 format", MultivaluedSupport.MULTIVALUED);
	}
	
	@Override
	public LogicalModel importFile(File f) throws IOException {
		return new AvatarImport(f).getModel();
	}
	
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException, AvatarLogicalModelException {
		new AvatarExport(model).export(out);
	}

}
