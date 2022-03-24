package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.constants.Qualifier;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * One instance per component (model, node, transition...)
 * It contains the annotations and the notes relative to a component
 *
 * @author Martin Boutroux
 * @author Aurelien Naldi
 */
public class Metadata {

    private final Map<Qualifier, List<Annotation>> annotations;
    private String notes;


    public Metadata() {
        this.annotations = new HashMap<>();
    }

    public boolean isEmpty() {
        return this.annotations.isEmpty() && this.notes.isEmpty();
    }

    public JSONObject toJSON() {
        if (this.isEmpty()) {
            return null;
        }
        JSONObject json = new JSONObject();
        if (!annotations.isEmpty()) {
            JSONObject jsonAnnot = new JSONObject();

            JSONArray arrayQualifiers = new JSONArray();

            for (Map.Entry<Qualifier, List<Annotation>> e: this.annotations.entrySet()) {
                Qualifier qualifier = e.getKey();
                List<Annotation> annotations = e.getValue();
                if (annotations.isEmpty()) {
                    continue;
                }

                JSONArray arrayAlternatives = new JSONArray();
                for (Annotation annot: annotations) {
                    JSONObject jsonAlternative = annot.getJSONOfAnnotation();
                    arrayAlternatives.put(jsonAlternative);
                }

                // FIXME: also export nested annotations

                if (!arrayAlternatives.isEmpty()) {
                    jsonAnnot.put(qualifier.term, arrayAlternatives);
                    arrayQualifiers.put(jsonAnnot);
                }
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

    public Iterable<Qualifier> qualifiers() {
        return this.annotations.keySet();
    }

    public Annotation getAnnotation(Qualifier qualifier, int alternative) {
        List<Annotation> qualified = this.annotations.get(qualifier);
        if (qualified == null) {
            return null;
        }
        return qualified.get(alternative);
    }

    public Annotation ensureAnnotation(Qualifier qualifier, int alternative) {
        List<Annotation> qualified = this.annotations.computeIfAbsent(qualifier, k -> new ArrayList<>());
        if (qualified.isEmpty()) {
            qualified.add(new Annotation());
        }
        return qualified.get(0);
    }

    public void fromJSON(JSONObject json) {
        // FIXME: extract annotations from the JSON object
    }
}
