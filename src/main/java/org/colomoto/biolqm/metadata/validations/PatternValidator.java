package org.colomoto.biolqm.metadata.validations;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PatternValidator {

    private static final Pattern EMAIL = Pattern.compile("^(.+)@(.+)$");
    private static final Pattern ORCID = Pattern.compile("\\d{4}-\\d{4}-\\d{4}-\\d{4}");

    private static final Pattern TAG = Pattern.compile("^#([a-zA-Z][a-zA-Z0-9]*)$");
    private static final Pattern COLLECTION = Pattern.compile("^([a-zA-Z][a-zA-Z0-9]*):(.+)$");

    public static boolean isValidEmail(String s) {
        return s == null || EMAIL.matcher(s).matches();
    }

    public static boolean isValidORCID(String s) {
        return s ==  null || ORCID.matcher(s).matches();
    }

    public static Matcher matchTag(String s) {
        if (s == null) {
            return TAG.matcher("");
        }
        return TAG.matcher(s);
    }

    public static Matcher matchCollection(String s) {
        if (s == null) {
            return COLLECTION.matcher("");
        }
        return COLLECTION.matcher(s);
    }


    public void guessAnnotationType(String s) {
        if (s == null || s.isEmpty()) {
            return;
        }

        if (TAG.matcher(s).matches()) {

        }
    }
}
