package org.colomoto.biolqm.io;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Provide OutputStreams from the file system
 *
 * @author Aurelien Naldi
 */
public class OutputStreamProviderFileImpl implements OutputStreamProvider {

    private final File parent;
    private final String filename;

    public OutputStreamProviderFileImpl(String filename) {
        this(new File(filename));
    }

    public OutputStreamProviderFileImpl(File file) {
        this.parent = file.getParentFile();
        this.filename = file.getName();
    }

    @Override
    public OutputStream getOutputStream(String pattern) throws IOException  {
        File f = parent;
        String[] path = pattern.split("/");
        for (String s: path) {
            s = s.replace("$f", filename);
            f = new File(f, s);
        }

        return new FileOutputStream(f);
    }
}
