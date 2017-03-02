package org.colomoto.biolqm.modifier.reduction;

public class ReductionSettings {

    public boolean handleFixed = false;
    public boolean purgeFixed = false;

    public boolean handleDuplicates = false;

    public boolean handleOutputs = false;

    public ReductionSettings() {
        this("");
    }

    public ReductionSettings(String parameters) {
        String[] options = parameters.split(",");
        for (String o: options) {
            if ("fixed".equalsIgnoreCase(o)) {
                handleFixed = true;
            } else if ("duplicate".equalsIgnoreCase(o)) {
                handleDuplicates = true;
            } else if ("output".equalsIgnoreCase(o)) {
                handleOutputs = true;
            }
        }
    }

}
