package org.colomoto.biolqm.io;

import java.io.*;

/**
 * Provide input streams from the file system.
 *
 * This implementation class should not be used directly, use the
 * static create methods in the StreamProvider interface instead.
 *
 * @author Aurelien Naldi
 */
public class StreamProviderFileImpl implements StreamProvider {

    private final File parent;
    private final String filename;

    StreamProviderFileImpl(String filename) {
        this(new File(filename));
    }

    StreamProviderFileImpl(File file) {
        this.parent = file.getParentFile();
        this.filename = file.getName();
    }

    @Override
    public InputStream input(String pattern) throws IOException  {
        return new FileInputStream(getFile(pattern));
    }

    @Override
    public OutputStream output(String pattern) throws IOException  {
        return new FileOutputStream(getFile(pattern));
    }

    private File getFile(String pattern) {
        File f = parent;
        String[] path = pattern.split("/");
        for (String s: path) {
            s = s.replace("$f", filename);
            f = new File(f, s);
        }
        return f;
    }

    @Override
    public File getFile() {
        return new File(parent, filename);
    }

}
