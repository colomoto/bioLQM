package org.colomoto.biolqm.metadata;

/**
 * Identify the type of annotated element
 */
public enum AnnotationTarget {
    Model,
    Component,
    Interaction;

    public static AnnotationTarget parse(String component) {
        switch (component) {
            case "model":
                return Model;
            case "species":
            case "component":
            case "node":
                return Component;
            case "edge":
            case "transition":
            case "interaction":
                return Interaction;
        }
        System.err.println("Unrecognized component type: "+ component);
        return null;
    }
}
