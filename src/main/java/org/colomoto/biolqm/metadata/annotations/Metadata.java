package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

/**
 * One instance per component (model, node, transition...)
 * It contains the annotations and the notes relative to a component
 *
 * @author Martin Boutroux
 * @author Aurelien Naldi
 */
public class Metadata {

    private final List<Annotation> annotations;
    private String notes;


    public Metadata() {
        this.annotations = new ArrayList<>();
    }

    public boolean isEmpty() {
        return this.annotations.isEmpty() && (this.notes == null || this.notes.isEmpty());
    }

    public JSONObject toJSON() {
        if (this.isEmpty()) {
            return null;
        }
        JSONObject json = new JSONObject();
        if (!annotations.isEmpty()) {
            JSONArray jsonAnnot = new JSONArray();

            for (Annotation annot: this.annotations) {
                // FIXME: also export nested annotations
                if (annot.isEmpty()) {
                    continue;
                }
                jsonAnnot.put(annot.toJSON());
            }
            json.put("annotation", jsonAnnot);
        }

        if (this.notes != null && !this.notes.isEmpty()) {
            json.put("notes", this.notes);
        }
        return json;
    }

    public String getNotes() {
        return this.notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public List<Annotation> annotations() {
        return this.annotations;
    }

    public Annotation getAnnotation(Qualifier qualifier, int alternative) {
        // FIXME: handle alternative
        for (Annotation annot: this.annotations) {
            if (annot.qualifier == qualifier) {
                return annot;
            }
        }
        return null;
    }

    public Annotation ensureAnnotation(Qualifier qualifier, int alternative) {
        Annotation result = this.getAnnotation(qualifier, alternative);
        if (result == null) {
            // FIXME: handle alternative
            result = new Annotation(qualifier);
            this.annotations.add(result);
        }
        return result;
    }
}
