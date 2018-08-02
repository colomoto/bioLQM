package org.colomoto.biolqm.io;

import java.io.*;

/**
 * Provide InputStream objects to import filters.
 *
 * @author Aurelien Naldi
 */
public interface InputStreamProvider {

    /**
     * Get the default stream.
     *
     * @return the input stream
     * @throws IOException if creating the stream failed
     */
    default InputStream getInputStream() throws IOException {
        return getInputStream("$f");
    }

    /**
     * Get a custom stream, specifying a pattern.
     * The pattern enables to change the file extension, write multiple files to a sub-folder...
     * Available patterns:
     * <ul>
     * <li>$f: the selected filename</li>
     * </ul>
     * @param pattern a naming pattern for the new stream
     * @return an input stream associated to the desired pattern
     * @throws IOException if creating the stream failed
     */
    InputStream getInputStream(String pattern) throws IOException;

    default Reader getReader() throws IOException {
        return new InputStreamReader( getInputStream());
    }

    default Reader getReader(String pattern) throws IOException {
        return new InputStreamReader( getInputStream(pattern));
    }

    /**
     * Close the stream provider
     */
    default void close() {
    }

    @Deprecated
    default File getFile() {
        throw new UnsupportedOperationException();
    }
}
