package org.colomoto.logicalmodel.io.sbml;

import java.io.File;
import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.sbml.jsbml.Model;
import org.sbml.jsbml.SBMLDocument;
import org.sbml.jsbml.ext.SBasePlugin;
import org.sbml.jsbml.ext.layout.LayoutConstants;
import org.sbml.jsbml.ext.qual.QualConstant;
import org.sbml.jsbml.ext.qual.QualitativeModel;
import org.sbml.jsbml.xml.stax.SBMLReader;

public class SBMLqualHelper {

	public static SBMLQualBundle loadFile(File f) throws IOException, XMLStreamException {
		return getQualitativeModel( parseFile(f));
	}
	
	public static SBMLDocument parseFile(File f) throws IOException, XMLStreamException {
		
		return new SBMLReader().readSBML(f);
	}
	
	public static SBMLQualBundle newBundle() {
		// init SBML document
		SBMLDocument sdoc = new SBMLDocument(3,1);
		sdoc.addNamespace(QualConstant.shortLabel, "xmlns", QualConstant.namespaceURI);
		sdoc.addNamespace(LayoutConstants.shortLabel, "xmlns", LayoutConstants.namespaceURI);

		// create SBML and qual models
		Model smodel = sdoc.createModel("model_id");
		QualitativeModel qmodel = new QualitativeModel(smodel);
		smodel.addExtension(QualConstant.namespaceURI, qmodel);

		return new SBMLQualBundle(sdoc, smodel, qmodel);
	}
	
	
	public static SBMLQualBundle getQualitativeModel(SBMLDocument sdoc) {
		Model smodel = sdoc.getModel();
		
		// Warning: how will we deal with multiple namespace versions?
		QualitativeModel qmodel = null;
		SBasePlugin plugin = smodel.getExtension(QualConstant.namespaceURI);
		if (plugin instanceof QualitativeModel) {
			qmodel = (QualitativeModel)plugin;
		}

		
		return new SBMLQualBundle(sdoc, smodel, qmodel);
	}
}
