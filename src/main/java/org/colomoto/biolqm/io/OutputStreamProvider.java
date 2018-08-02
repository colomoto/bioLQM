package org.colomoto.biolqm.io;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Provide OutputStream objects to export filters.
 *
 * @author Aurelien Naldi
 */
public interface OutputStreamProvider {

    /**
     * Get a single stream. When saving to a file it will have the selected filename.
     *
     * @return the output stream
     * @throws IOException if creating the stream failed
     */
    default OutputStream getOutputStream() throws IOException {
        return getOutputStream("$f");
    }

    /**
     * Get a custom stream, specifying a pattern.
     * The pattern enables to change the file extension, write multiple files to a sub-folder...
     * Available patterns:
     * <ul>
     * <li>$f: the selected filename</li>
     * </ul>
     * @param pattern a naming pattern for the new stream
     * @return an output stream associated to the desired pattern
     * @throws IOException if creating the stream failed
     */
    OutputStream getOutputStream(String pattern) throws IOException;

    default void close() {}

}
