package org.colomoto.logicalmodel.services;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;

/**
 * Helper class to support extensions
 */
public class ExtensionLoader {

    private static ClassLoader cld = null;

    /**
     * Retrieve a classloader supporting extensions
     * @return the extended classloader
     */
    public static ClassLoader getClassLoader() {
        return cld;
    }

    /**
     * Trigger the loading of extensions. This must be called only once, before all use of getClassLoader()
     *
     * @param ename
     * @param cl
     */
    public static void loadExtensions(String ename, Class cl) {
        if (cld != null) {
            System.err.print("Extensions are already loaded");
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

        // extend the classpath if needed
        File extensionDir = new File(basedir, "extensions");
        if (!extensionDir.isDirectory()) {
            cld = cl.getClassLoader();
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

            for (URL u: urls) {
                System.out.println("EXT: " + u);
            }
            System.out.println();
            cld = new URLClassLoader(urls);
        } catch (IOException e) {
            System.err.println("Could not load extension files");
            cld = cl.getClassLoader();
        }

    }

}
