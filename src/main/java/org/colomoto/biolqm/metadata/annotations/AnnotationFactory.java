package org.colomoto.biolqm.metadata.annotations;

import org.colomoto.biolqm.metadata.annotations.Annotation;
import org.colomoto.biolqm.metadata.annotations.GenericAnnotation;
import org.colomoto.biolqm.metadata.annotations.AuthorsAnnotation;
import org.colomoto.biolqm.metadata.annotations.DateAnnotation;
import org.colomoto.biolqm.metadata.annotations.DistributionAnnotation;

/**
 * This produces an instance of the appropriate class considering the type of annotation being created
 *
 * @author Martin Boutroux
 */
class AnnotationFactory {
	
	// functions
	protected Annotation getInstance(String nameClass) {
		switch (nameClass) {
			case "GenericAnnotation":
				return new GenericAnnotation();
			case "AuthorsAnnotation":
				return new AuthorsAnnotation();
			case"DateAnnotation":
				return new DateAnnotation();
			case "DistributionAnnotation":
				return new DistributionAnnotation();
		}
		
		return null;
	}
}
