package org.colomoto.biolqm.metadata.validations;

import java.text.DateFormat;  
import java.text.SimpleDateFormat;  
import java.text.ParseException;

/**
 * Useful to check if a date follows the standard of the W3C
 *
 * @author Martin Boutroux
 */
public class DateValidator {
    private String dateFormat;

    public DateValidator(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    public boolean isValid(String dateStr) {
        DateFormat sdf = new SimpleDateFormat(this.dateFormat);
        sdf.setLenient(false);
        try {
            sdf.parse(dateStr);
        } catch (ParseException e) {
            return false;
        }
        return true;
    }
}