package org.colomoto.biolqm.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Provide input streams from the file system
 *
 * @author Aurelien Naldi
 */
public class InputStreamProviderFileImpl implements InputStreamProvider {

    private final File parent;
    private final String filename;

    public InputStreamProviderFileImpl(String filename) {
        this(new File(filename));
    }

    public InputStreamProviderFileImpl(File file) {
        this.parent = file.getParentFile();
        this.filename = file.getName();
    }

    @Override
    public InputStream getInputStream(String pattern) throws IOException  {
        File f = parent;
        String[] path = pattern.split("/");
        for (String s: path) {
            s = s.replace("$f", filename);
            f = new File(f, s);
        }

        return new FileInputStream(f);
    }
}
