package org.colomoto.biolqm.modifier.sanitize;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;

import java.text.Normalizer;
import java.util.*;
import java.util.regex.Pattern;

/**
 *
 * @author Aurelien Naldi
 */
public class SanitizeModifier extends BaseModifier {

    private final LogicalModel model;
    private boolean sanitizeIDs = true;
    private final Map<String, NodeInfo> uids = new HashMap<>();

    public SanitizeModifier(LogicalModel model) {
        this.model = model;
    }

    private static final Pattern NONLATIN = Pattern.compile("[^\\w_]");
    private static final Pattern WHITESPACE = Pattern.compile("[\\s]");

    @Override
    public LogicalModel performTask() {

        if (sanitizeIDs) {
            sanitizeIDs(model.getComponents());
            sanitizeIDs(model.getExtraComponents());
        }

        return model;
    }

    @Override
    public void setParameters(String[] options) {
        for (String o: options) {
            boolean b = true;
            if (o.startsWith(":")) {
                o = o.substring(1);
            } else if (o.startsWith("!") || o.startsWith("^")) {
                o = o.substring(1);
                b = false;
            }
            if ("name2id".equalsIgnoreCase(o)) {
                setSanitizeIDs(b);
            }
        }
    }

    public boolean isSanitizeIDs() {
        return sanitizeIDs;
    }

    public void setSanitizeIDs(boolean b) {
        sanitizeIDs = b;
    }

    private void sanitizeIDs(List<NodeInfo> components) {

        for (NodeInfo ni : components) {
            // Pick a better ID?
            String name = ni.getName();
            if (name == null || name.length() == 0) {
                // This component has no name, keep it's ID
                String uid = ni.getNodeID();
                NodeInfo oni = uids.get(uid);
                uids.put(uid, ni);

                // If a previous component tried to steal this ID, clean it up
                if (oni != null) {
                    ensureUniqueID(oni);
                }
            } else {
                ensureUniqueID(ni);
            }
        }
    }

    private void ensureUniqueID(NodeInfo ni) {
        String uid = ni.getNodeID();
        String name = ni.getName();

        // This function should only be called with named components, but better be safe
        if (name != null && name.length() > 0) {
            String nowhitespace = WHITESPACE.matcher(name).replaceAll("_");
            String normalized = Normalizer.normalize(nowhitespace, Normalizer.Form.NFD);
            uid = NONLATIN.matcher(normalized).replaceAll("");
        }

        // Ensure uniqueness
        String baseUid = uid;
        int tag = 1;
        while (uids.containsKey(uid)) {
            tag++;
            uid = baseUid + "_" + tag;
        }

        ni.setNodeID(uid);
        uids.put(uid, ni);
    }

}
