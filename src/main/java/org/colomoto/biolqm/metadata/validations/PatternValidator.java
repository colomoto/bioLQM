package org.colomoto.biolqm.metadata.validations;

import java.util.AbstractMap;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PatternValidator {

    private static final Pattern EMAIL = Pattern.compile("^(.+)@(.+)$");
    private static final Pattern ORCID = Pattern.compile("\\d{4}-\\d{4}-\\d{4}-\\d{4}");

    private static final Pattern QUALIFIER = Pattern.compile("^(?:@)([a-zA-Z][a-zA-Z0-9_]*)$");
    private static final Pattern TAG = Pattern.compile("^(?:#|tag:)([a-zA-Z][a-zA-Z0-9]*)$");
    private static final Pattern KEYVAL = Pattern.compile("^(?:keyvalue:)?([a-zA-Z][a-zA-Z0-9]*)=([a-zA-Z0-9]+)$");
    private static final Pattern COLLECTION = Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*):(.+)$");

    public static boolean isValidEmail(String s) {
        return s == null || EMAIL.matcher(s).matches();
    }

    public static boolean isValidORCID(String s) {
        return s ==  null || ORCID.matcher(s).matches();
    }

    public static Optional<String> asTag(String s) {
        if (s == null || s.isBlank()) {
            return Optional.empty();
        }
        Matcher m = TAG.matcher(s);
        if (m.matches()) {
            return Optional.of(m.group(1));
        }
        return Optional.empty();
    }
    public static Optional<AbstractMap.SimpleImmutableEntry<String,String>> asKeyValue(String s) {
        if (s == null || s.isBlank()) {
            return Optional.empty();
        }
        Matcher m = KEYVAL.matcher(s);
        if (m.matches()) {
            return Optional.of(new AbstractMap.SimpleImmutableEntry<>(m.group(1), m.group(2)));
        }
        return Optional.empty();
    }

    public static Optional<AbstractMap.SimpleImmutableEntry<String,String>> asCollectionEntry(String s) {
        if (s == null || s.isBlank()) {
            return Optional.empty();
        }
        Matcher m = COLLECTION.matcher(s);
        if (m.matches()) {
            return Optional.of(new AbstractMap.SimpleImmutableEntry<>(m.group(1), m.group(2)));
        }
        return Optional.empty();
    }

    public static Optional<String> asQualifier(String s) {
        if (s == null || s.isBlank()) {
            return Optional.empty();
        }
        Matcher m = QUALIFIER.matcher(s);
        if (m.matches()) {
            return Optional.of(m.group(1));
        }
        return Optional.empty();
    }

    public static Type validate(String s) {
        if (s == null || s.isBlank()) {
            return Type.EMPTY;
        }

        if (TAG.matcher(s).matches()) {
            return Type.TAG;
        }

        if (QUALIFIER.matcher(s).matches()) {
            return Type.QUALIFIER;
        }

        if (KEYVAL.matcher(s).matches()) {
            return Type.KEY_VALUE;
        }
        if (COLLECTION.matcher(s).matches()) {
            return Type.COLLECTION;
        }

        return Type.INVALID;
    }

    public enum Type {
        QUALIFIER, TAG, KEY_VALUE, COLLECTION, INVALID, EMPTY
    }
}
