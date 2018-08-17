package org.colomoto.biolqm.modifier.submodel;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.ReferenceModels;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test sub-model extraction.
 *
 * @author Celine Hernandez
 */
public class TestSubModel {
    
    /**
     * Test different configurations.
     *
     * @throws Exception
     */
    @Test
    public void testParameters() throws Exception {
        for (String name: ReferenceModels.getNames()) {
            // Load model
            LogicalModel model = ReferenceModels.getModel(name);
            assertNotNull(model,
                    "Error loading model "+name);
            
            
            // Test: calling performTask() before setParameters() or any parameters initializer
    
            // Creation of the modifier
            SubmodelModifier modifierWrongOrder = new SubmodelModifier(model);
            assertNotNull(modifierWrongOrder,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: null
            assertThrows((NullPointerException.class),
                    modifierWrongOrder::performTask,
                    "Not expected to call performTask() before setParameters().");
    
            
            // Null parameters
            
            // Creation of the modifier
            SubmodelModifier modifierNull = new SubmodelModifier(model);
            assertNotNull(modifierNull,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: null
            assertThrows((NullPointerException.class),
                    ()-> modifierNull.setParameters((String[]) null),
                    "Expected to raise an exception when providing a null set of parameters.");
    
            
            // Empty set of parameters
            
            // Creation of the modifier
            SubmodelModifier modifierEmpty = new SubmodelModifier(model);
            assertNotNull(modifierEmpty,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: empty
            assertDoesNotThrow(()-> modifierEmpty.setParameters(new String[0]),
                    "Should not raise an exception when providing an empty set of parameters.");
            // Perform task
            LogicalModel afterTask = modifierEmpty.performTask();
            assertNotNull(afterTask, "Null model returned after providing empty set of parameters.");
            assertSame(model, afterTask, "Before/after models are not the same.");
            
        }
    }
    
    /**
     *
     * @throws Exception
     */
    @Test
    public void testTask() throws Exception {
        for (String name: ReferenceModels.getNames()) {
            // Load model
            LogicalModel model = ReferenceModels.getModel(name);
            assertNotNull(model,
                    "Error loading model "+name);
            
            // All components of the model
            List<NodeInfo> allModelComponents = model.getComponents();
            assertNotNull(allModelComponents,
                    "Loaded model "+name+" has no components.");
            assertTrue(allModelComponents.size() > 0,
                    "Loaded model "+name+" has "+allModelComponents.size()+" component.");
            
            // All IDs
            List<String> compIDs = new ArrayList<>(allModelComponents.size());
            for (NodeInfo ni : allModelComponents) {
                compIDs.add(ni.getNodeID());
            }
            
            // Create a list of component names not already in the model
            List<String> parametersNone = Arrays.asList("HopeThisOneNodeDoesNotExist", "HopeNeitherDoesThisOne");
            parametersNone.removeAll( compIDs );
            assertTrue(parametersNone.size() > 0,
                    "Parameters to be tested are all in model "+name+". Please edit this test.");
            
            
            // Tests
            
            
            // None of the parameters belong to the model
    
            // Creation of the modifier
            SubmodelModifier modifierNone = new SubmodelModifier(model);
            assertNotNull(modifierNone,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: none belong to the model
            assertDoesNotThrow(()-> modifierNone.setParameters(parametersNone.toArray(new String[0])),
                    "Should not raise an exception when providing only parameters not in the model. Only a warning.");
            // Perform task
            LogicalModel afterTaskNone = modifierNone.performTask();
            assertNotNull(afterTaskNone, "Null model returned after providing parameters not in the model.");
            assertSame(model, afterTaskNone, "Before/after models are not the same.");
            
            
            // One parameter doesn't belong to the model
    
            // Creation of the modifier
            SubmodelModifier modifierOne = new SubmodelModifier(model);
            assertNotNull(modifierOne,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: mix of names in model or not
            String[] parametersOne = new String[] {parametersNone.get(0), compIDs.get(compIDs.size()-1)};
            assertDoesNotThrow(()-> modifierOne.setParameters(parametersOne),
                    "Should not raise an exception when providing a mix of parameters in or not in the model.");
            // Perform task
            LogicalModel afterTaskOne = modifierOne.performTask();
            assertNotNull(afterTaskOne, "Null model returned after providing a mix of parameters in or not in the model.");
            assertNotSame(model, afterTaskOne, "Before/after models should not be the same.");
            assertTrue(afterTaskOne.getComponents().size() <= model.getComponents().size(),
                    "New model should not have more components than the original one.");
    
    
            // All parameters are asked to be extracted
    
            // Creation of the modifier
            SubmodelModifier modifierAll = new SubmodelModifier(model);
            assertNotNull(modifierAll,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: all components
            assertDoesNotThrow(()-> modifierAll.setParameters(compIDs.toArray(new String[0])),
                    "Should not raise an exception when providing all component IDs in the model.");
            // Perform task
            LogicalModel afterTaskAll = modifierAll.performTask();
            assertNotNull(afterTaskAll, "Null model returned after providinga all component IDs in the model.");
            assertNotSame(model, afterTaskAll, "Before/after models should not be the same.");
            assertEquals(afterTaskAll.getComponents().size(), model.getComponents().size(),
                    "New model should have the same number of components than the original one. New:"+afterTaskAll.getComponents().size()+" vs old:"+model.getComponents().size());
    
    
            // Some provided parameters are duplicated
    
            // Creation of the modifier
            SubmodelModifier modifierNotUnique = new SubmodelModifier(model);
            assertNotNull(modifierNotUnique,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: all components
            String[] parametersNotUnique = new String[] {compIDs.get(0), compIDs.get(0)};
            assertDoesNotThrow(()-> modifierNotUnique.setParameters(parametersNotUnique),
                    "Should not raise an exception when providing repeated component IDs.");
            // Perform task
            LogicalModel afterTaskNotUnique = modifierNotUnique.performTask();
            assertNotNull(afterTaskNotUnique, "Null model returned after providing repeated component IDs.");
            assertNotSame(model, afterTaskNotUnique, "Before/after models should not be the same.");
            assertTrue(afterTaskNotUnique.getComponents().size() <= model.getComponents().size(),
                    "New model should not have more components than the original one. New:"+afterTaskNotUnique.getComponents().size()+" vs old:"+model.getComponents().size());
    
    
            // Expected usage : we select two first components names and create a model with at least these two components.
    
            // Creation of the modifier
            SubmodelModifier modifierExpected = new SubmodelModifier(model);
            assertNotNull(modifierExpected,
                    "Could not create instance of SubmodelModifier with model "+name+".");
            // Set parameters: two components
            String[] parametersExpected = new String[] {compIDs.get(0), compIDs.get(1)};
            assertDoesNotThrow(()-> modifierExpected.setParameters(parametersExpected),
                    "Should not raise an exception when providing component names in the model.");
            // Perform task
            LogicalModel afterTaskExpected = modifierExpected.performTask();
            assertNotNull(afterTaskExpected, "Null model returned after providing component names in the model.");
            assertNotSame(model, afterTaskExpected, "Before/after models should not be the same.");
            assertTrue(afterTaskExpected.getComponents().size() <= model.getComponents().size(),
                    "New model should not have more components than the original one.");
            assertTrue(afterTaskExpected.getComponents().size() >= 2,
                    "New model should have at least two components.");
            
        }
    }
    
    
}
