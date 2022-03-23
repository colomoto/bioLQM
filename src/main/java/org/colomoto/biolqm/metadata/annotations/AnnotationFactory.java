package org.colomoto.biolqm.metadata.annotations;

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
