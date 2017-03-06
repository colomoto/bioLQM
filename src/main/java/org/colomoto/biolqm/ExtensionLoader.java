package org.colomoto.biolqm;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Iterator;
import java.util.ServiceLoader;

/**
 * Helper class to support extensions.
 *
 * @author Aurelien Naldi
 */
public class ExtensionLoader {

    private static ClassLoader contextLoader = Thread.currentThread().getContextClassLoader();
    private static ClassLoader cld = null;

    /**
     * Retrieve a classloader covering the extensions.
     *
     * @return the extended classloader
     */
    public static ClassLoader getClassLoader() {
        if (cld == null) {
            return contextLoader;
        }
        return cld;
    }

    /**
     * Trigger the loading of extensions. This must be called only once, before all use of getClassLoader()
     *
     * @param ename name of the optional extension folder
     * @param cl caller class (needed to find the basename)
     */
    public static void loadExtensions(String ename, Class cl) {
        if (cld != null) {
            System.err.println("Extensions are already loaded");
            return;
        }

        String basedir = null;

        String clname = cl.getName().replace(".",	"/") + ".class";
        String path = cl.getClassLoader().getResource(clname).toString();
        if (path.startsWith("file:")) {
            basedir = path.substring(5,  path.length() - clname.length());
        } else if (path.startsWith("jar:file:")) {
            File jar = new File(path.substring(9,  path.length() - clname.length() - 2));
            basedir = jar.getParent();
        }

        // extend the classpath if an extension folder is available
        if (ename == null) {
            cld = contextLoader;
            return;
        }
        
        File extensionDir = new File(basedir, ename);
        if (!extensionDir.isDirectory()) {
            cld = contextLoader;
            return;
        }


        FileFilter filter = new FileFilter() {
            public boolean accept(File file) {return file.getPath().toLowerCase().endsWith(".jar");}
        };
        File[] files = extensionDir.listFiles();
        try {
            URL[] urls = new URL[files.length];
            int i=0;
            for (File f: files) {
                urls[i++] = f.toURI().toURL();
            }

            cld = new URLClassLoader(urls, contextLoader);
        } catch (IOException e) {
            System.err.println("Could not load extension files");
            cld = contextLoader;
        }

    }

    /**
     * Provide a ServiceLoader which will use the extended classpath if available.
     *
     * @param cl the class to load
     * @return a ServiceLoader: iterable list of matching classes
     */
    public static ServiceLoader load(Class cl) {
        return ServiceLoader.load(cl, getClassLoader());
    }

    /**
     * Discover services using the extended classpath.
     *
     * @param cl the class to load
     * @return an iterator over the matching classes
     */
    public static Iterator iterator(Class cl) {
        return load(cl).iterator();
    }

}
