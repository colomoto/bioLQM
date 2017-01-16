package org.colomoto.biolqm.io.avatar;

import java.io.IOException;

/** 
 * Exception associated with the loading and exportation of Avatar files
 * @author Rui Henriques
 * @version 1.0
 */
public class AvatarLogicalModelException extends IOException {

	private static final long serialVersionUID = 1;

	/**
	 * Creates a new Avatar exception with a dedicated message
     * @param message the detail message
     */
    public AvatarLogicalModelException(String message) {
        super(message);
    }

    /**
     * Creates a new Avatar exception with a detail message and nested exception
     * @param message the detail message
     * @param cause the nested exception
     */
    public AvatarLogicalModelException(String message, Throwable cause) {
        super(message, cause);
    }
}
