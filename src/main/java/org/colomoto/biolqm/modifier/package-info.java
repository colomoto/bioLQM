/**
 * Modifications of logical models.
 *
 * This package defines {@link org.colomoto.biolqm.modifier.ModelModifier} interface.
 * Each model modifier is a task taking a model (and some parameters) as input and
 * constructing a new model derived from it.
 *
 * These modifiers are provided through service-discovery, see {@link org.colomoto.biolqm.modifier.ModelModifierService}.
 */
package org.colomoto.biolqm.modifier;