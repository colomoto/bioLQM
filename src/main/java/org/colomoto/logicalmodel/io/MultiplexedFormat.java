package org.colomoto.logicalmodel.io;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Convenience class to wrap a specific subformat into a "standard" format instance.
 * 
 * @author Aurelien Naldi
 *
 * @param <T> the type of subformat
 */
public class MultiplexedFormat<T extends Enum> implements LogicalModelFormat {

	private final FormatMultiplexer<T> format;
	private final T subformat;
	
	public MultiplexedFormat(FormatMultiplexer<T> format, T subformat) {
		this.format = format;
		this.subformat = subformat;
	}
	public MultiplexedFormat(FormatMultiplexer<T> format, String subformat) {
		this(format, format.getSubformat(subformat));
	}
	@Override
	public String getID() {
		return format.getID()+"@"+subformat;
	}
	@Override
	public String getName() {
		return format.getName();
	}
	@Override
	public boolean canExport() {
		return format.canExport();
	}
	@Override
	public boolean canImport() {
		return format.canImport();
	}
	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		format.export(model, out, subformat);
	}
	@Override
	public LogicalModel importFile(File f) throws IOException {
		return format.importFile(f, subformat);
	}
}
