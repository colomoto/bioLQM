package org.colomoto.biolqm.metadata.constants;

import org.yaml.snakeyaml.Yaml;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class QualifierGroup {

    private final String groupName;
    private final Map<String, Qualifier> qualifiers;

    public QualifierGroup(String name) {
        this.groupName = name;
        this.qualifiers = new HashMap<>();
    }

    public QualifierGroup(String name, InputStream inputStream) {
        this(name);

        Yaml yaml = new Yaml();
        ArrayList<Map<String, Object>> termsOntology = yaml.load(inputStream);
        for (Map<String, Object> caracs : termsOntology) {
            String newTerm = (String) caracs.get("term");
            String newDescription = (String) caracs.get("description");
            String newDefinition = (String) caracs.get("definition");
            this.add(new Qualifier(newTerm, newDescription, newDefinition));
        }
    }

    public void add(Qualifier q) {
        this.qualifiers.put(q.term, q);
    }

    public Qualifier get(String name) {
        return this.qualifiers.get(name);
    }

    public Qualifier ensure(String name) {
        return this.qualifiers.computeIfAbsent(name, Qualifier::new);
    }

    public Stream<Qualifier> suggestions(String skel) {
        return this.qualifiers.values().stream().filter(q -> q.matches(skel));
    }
}
