package org.colomoto.logicalmodel.io;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;

public class AbstractFormatMultiplexer<T extends Enum> extends AbstractFormat implements FormatMultiplexer<T> {

	private final T[] subformats;
	
	protected AbstractFormatMultiplexer(String id, String name, T[] subformats) {
		this(id, name, false, subformats);
	}
	
	protected AbstractFormatMultiplexer(String id, String name, boolean supportsMultivalued, T[] subformats) {
		super(id, name, supportsMultivalued);
		this.subformats = subformats;
	}

	@Override
	public LogicalModel importFile(File f) throws IOException {
		return importFile(f, subformats[0]);
	}

	@Override
	public void export(LogicalModel model, OutputStream out) throws IOException {
		export(model, out, subformats[0]);
	}

	
	@Override
	public void export(LogicalModel model, OutputStream out, T subformat) throws IOException {
		throw new RuntimeException("Export not implemented for format " + getID());
	}

	@Override
	public LogicalModel importFile(File f, T subformat) throws IOException {
		throw new RuntimeException("Import not implemented for format " + getID());
	}

	@Override
	public T[] getSubformats() {
		return subformats;
	}

	@Override
	public T getSubformat(String id) {
		T sub = subformats[0];
		return (T)sub.valueOf(sub.getClass(), id);
	}
}
