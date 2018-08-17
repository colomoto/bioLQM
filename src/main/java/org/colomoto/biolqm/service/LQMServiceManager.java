package org.colomoto.biolqm.service;

import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.colomoto.biolqm.LQMLauncher;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.modifier.booleanize.BooleanizeService;
import org.colomoto.biolqm.tool.ModelToolService;

/**
 * Static service manager: list available "services" in LQM:
 * <ul>
 *   <li>formats</li>
 *   <li>modifiers</li>
 * </ul>
 * @author Aurelien Naldi
 */
public class LQMServiceManager {

    private static final List<LogicalModelFormat> formats = ExtensionLoader.load_instances(LogicalModelFormat.class);
    private static final List<ModelToolService> tools = ExtensionLoader.load_instances(ModelToolService.class);
    private static final List<ModelModifierService> modifiers = ExtensionLoader.load_instances(ModelModifierService.class);

    private static final Map<String, LogicalModelFormat> id2format = new HashMap<String, LogicalModelFormat>();
    private static final Map<String, ModelToolService> id2tool = new HashMap<String, ModelToolService>();
    private static final Map<String, ModelModifierService> id2modifier = new HashMap<String, ModelModifierService>();

    private static Map<Class, Service> byClass = new HashMap<>();

    static {
        loadServices(formats, id2format);
        loadServices(tools, id2tool);
        loadServices(modifiers, id2modifier);
    }

    public final String[] args;

    /**
     * Create an instance, storing command-line arguments
     *
     * @param args the list of command-line arguments for use in scripts
     */
    public LQMServiceManager(String[] args) {
        this.args = args;
    }


    private static <T extends Service> void loadServices(List<T> services, Map<String, T> idmap) {
        for (T srv: services) {
            idmap.put( srv.getID(), srv);
            byClass.put(srv.getClass(), srv);
        }
        fillAliases(idmap, services);
    }

    private static <T extends Service> void fillAliases( Map<String, T> map, List<T> services) {
        for (T srv: services) {
            String[] aliases = srv.getAliases();
            if (aliases != null) {
                for (String alias: aliases) {
                    if (map.containsKey(alias)) {
                        continue;
                    }
                    map.put( alias, srv);
                }
            }

        }
    }

    /**
     * Get the format declaration for a given ID.
     *
     * @param name ID of the format
     * @return the format declaration instance or null if not found.
     */
    public static LogicalModelFormat getFormat(String name) {
        return id2format.get(name);
    }

    /**
     * Get the available formats.
     *
     * @return all available formats
     */
    public static Iterable<LogicalModelFormat> getFormats() {
        return formats;
    }

    /**
     * Get the tool declaration for a given ID.
     *
     * @param name ID of the tool
     * @return the tool declaration instance or null if not found.
     */
    public static ModelToolService getTool(String name) {

        return id2tool.get(name);
    }

    /**
     * Get the available tools.
     *
     * @return all available tools
     */
    public static Iterable<ModelToolService> getTools() {
        return tools;
    }

    /**
     * Get the service for a given ID.
     *
     * @param name ID of the service
     * @return the service instance or null if not found.
     */
    public static ModelModifierService getModifier(String name) {
        return id2modifier.get(name);
    }

    /**
     * Retrieve any service by class
     *
     * @param cl the class of the desired service
     * @param <T> the type of the desired service
     * @return an instance of this service, or null if it does not exist
     */
    public static <T> T get(Class<T> cl) {
        return (T)byClass.get(cl);
    }

    /**
     * Get the available model modifier services.
     *
     * @return all available services for model modifiers
     */
    public static Iterable<ModelModifierService> getModifiers() {
        return modifiers;
    }

    /**
     * Register a service after initial service discovery.
     *
     * When possible, service discovery should be used instead.
     *
     * @param srv the new service to register
     * @return true is the service was added
     */
    public static boolean register(Service srv) {
        if (srv == null) {
            return false;
        }

        if (register(ModelModifierService.class, srv, modifiers, id2modifier)) {
            return true;
        }
        if (register(ModelToolService.class, srv, tools, id2tool)) {
            return true;
        }
        if (register(LogicalModelFormat.class, srv, formats, id2format)) {
            return true;
        }

        return false;
    }

    private static <T> boolean register(Class<T> cl, Service srv, List<T> services, Map<String,T> id2srv) {
        String id = srv.getID();
        if (!cl.isInstance(srv) || id2srv.containsKey(id)) {
            return false;
        }

        services.add((T)srv);
        id2srv.put(id, (T)srv);
        return true;
    }


    /**
     * Load a model guessing the format.
     *
     * @param filename the path to the file to load (extension gives the format)
     * @return the loaded model
     */
    public static LogicalModel load(String filename) {
        return load(filename, null);
    }

    /**
     * Construct a modified model.
     * @see #modify(LogicalModel,String,String)
     *
     * @param model the original model
     * @param name the name of a modifier
     * @return a modified model
     * @throws Exception in case of error (following the Task interfce)
     */
    public static LogicalModel modify(LogicalModel model, String name) throws Exception {
        return getModifier(name).modify(model);
    }

    /**
     * Construct a modified model.
     * @see ModelModifierService#modify(LogicalModel,String)
     *
     * @param model the original model
     * @param name the name of a modifier
     * @param parameters parameters for the model modifier
     * @return a modified model
     * @throws Exception in case of error (following the Task interfce)
     */
    public static LogicalModel modify(LogicalModel model, String name, String parameters) throws Exception {
        return getModifier(name).modify(model, parameters);
    }

    /**
     * Open a file for writing.
     * A simple wrapper for FileWrapper constructor, to be used in scripts
     *
     * @param path the path to the file to open
     * @return a FileWriter object
     * @throws IOException if it could not be created
     */
    public static FileWriter fileWriter(String path) throws IOException {
        return new FileWriter(path);
    }

    /**
     * Load a model from file.
     *
     * @param filename the path to the loaded file
     * @param format the name of the import format
     * @return the loaded model
     */
    public static LogicalModel load(String filename, String format) {
        if (format == null) {
            format = filename.substring(filename.lastIndexOf(".")+1);
        }
        LogicalModelFormat inputFormat = LQMLauncher.getFormat(format);
        if (inputFormat == null) {
            System.err.println("Format not found: " + format);
            return null;
        }

        try {
            return inputFormat.load(filename);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Save a model to file
     *
     * @param model the model to save
     * @param filename the path of the output file
     * @param format the name of the export format
     * @return true if success
     */
    public static boolean save(LogicalModel model, String filename, String format) {
        if (format == null) {
            format = filename.substring(filename.lastIndexOf(".")+1);
        }
        LogicalModelFormat outputFormat = LQMLauncher.getFormat(format);
        if (outputFormat == null) {
            System.err.println("Format not found: "+format);
            return false;
        }

        try {
            if (!model.isBoolean()) {
                switch (outputFormat.getMultivaluedSupport()) {
                    case BOOLEAN_STRICT:
                        throw new RuntimeException(outputFormat.getID() +" does not support multivalued models");
                    case BOOLEANIZED:
                        System.out.println(outputFormat.getID() +": export of a booleanized model");
                        model = LQMServiceManager.get(BooleanizeService.class).modify(model);
                        break;
                }
            }

            outputFormat.export(model, filename);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;

    }
}
