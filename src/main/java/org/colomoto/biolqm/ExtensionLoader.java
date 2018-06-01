package org.colomoto.biolqm;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

/**
 * Helper class to support extensions.
 *
 * @author Aurelien Naldi
 */
public class ExtensionLoader {

    private static ClassLoader systemLoader = ClassLoader.getSystemClassLoader();
    private static ClassLoader cld = null;

    /**
     * Retrieve a class loader covering the extensions.
     *
     * @return the extended class loader
     */
    public static ClassLoader getClassLoader() {
        if (cld == null) {
            return systemLoader;
        }
        return cld;
    }

    /**
     * Trigger the loading of extensions. This must be called only once, before all use of getClassLoader()
     *
     * @param ename name of the optional extension folder
     * @param cl caller class (needed to find the basename)
     */
    public static void loadExtensions(String ename, Class<?> cl) {
        if (cld != null) {
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
            cld = systemLoader;
            return;
        }
        
        File extensionDir = new File(basedir, ename);
        if (!extensionDir.isDirectory()) {
            cld = systemLoader;
            return;
        }


//        System.out.println("Loading extensions from "+ extensionDir.getAbsolutePath());
        FileFilter filter = new FileFilter() {
            public boolean accept(File file) {return file.getPath().toLowerCase().endsWith(".jar");}
        };
        File[] files = extensionDir.listFiles(filter);
        try {
            URL[] urls = new URL[files.length];
            int i=0;
            for (File f: files) {
                urls[i++] = f.toURI().toURL();
                System.out.println("   "+ f.getName());
            }

            cld = new URLClassLoader(urls, systemLoader);
        } catch (IOException e) {
            System.err.println("Could not load extension files");
            cld = systemLoader;
        }
        if (cld != null && cld != systemLoader) {
            Thread.currentThread().setContextClassLoader(cld);
        }
    }

    /**
     * Provide a ServiceLoader which will use the extended classpath if available.
     *
     * @param cl the class to load
     * @return a ServiceLoader: iterable list of matching classes
     */
    private static <T> ServiceLoader<T> load(Class<T> cl) {
        return ServiceLoader.load(cl, getClassLoader());
    }

    /**
     * Discover and load a list of services.
     * 
     * @param <T> the type of the returned list of services
     * @param cl the base class to discover services (of type T)
     * @return the list of loaded instances (bypassing reflection and constructor errors)
     */
    public static <T> List<T> load_instances(Class<T> cl) {
        Iterator<T> it = load(cl).iterator();
        List<T> loaded = new ArrayList<T>();
        while (it.hasNext()) {
            try {
            	T element = it.next();
            	if (element != null) {
            		loaded.add(element);
            	}
            }
            catch (ServiceConfigurationError e){
            	// TODO: handle loading errors
            }
        }
        return loaded;
    }

}
