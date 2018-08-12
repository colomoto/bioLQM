package org.colomoto.biolqm.io;

import java.io.*;

/**
 * Provide Input or Output stream objects for the loading and saving operations filters.
 *
 * @author Aurelien Naldi
 */
public interface StreamProvider {

    /**
     * Get a single stream. When saving to a file it will have the selected filename.
     *
     * @return the output stream
     * @throws IOException if creating the stream failed
     */
    default OutputStream output() throws IOException {
        return output("$f");
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
    OutputStream output(String pattern) throws IOException;

    /**
     * Get the default stream.
     *
     * @return the input stream
     * @throws IOException if creating the stream failed
     */
    default InputStream input() throws IOException {
        return input("$f");
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
    InputStream input(String pattern) throws IOException;


    default Reader reader() throws IOException {
        return new InputStreamReader( input());
    }

    default Reader reader(String pattern) throws IOException {
        return new InputStreamReader( input(pattern));
    }

    default OutputStreamWriter writer() throws IOException {
        return new OutputStreamWriter( output());
    }
    default OutputStreamWriter writer(String pattern) throws IOException {
        return new OutputStreamWriter( output(pattern));
    }

    /**
     * Close this collection of streams.
     */
    default void close() {}

    @Deprecated
    default File getFile() {
        throw new UnsupportedOperationException();
    }

}
