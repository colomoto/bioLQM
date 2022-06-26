package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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

    public Optional<Annotation> getAnnotation(Qualifier qualifier) {
        return annotations
                .stream()
                .filter(annot -> annot.qualifier == qualifier)
                .findFirst();
    }

    public Annotation ensureAnnotation(Qualifier qualifier) {
        Optional<Annotation> result = this.getAnnotation(qualifier);
        if (result.isPresent()) {
            return result.get();
        }
        Annotation created = new Annotation(qualifier);
        this.annotations.add(created);
        return created;
    }
}
