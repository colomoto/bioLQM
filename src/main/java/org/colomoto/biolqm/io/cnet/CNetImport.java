package org.colomoto.biolqm.io.cnet;

import org.antlr.v4.runtime.*;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseLoader;
import org.colomoto.biolqm.io.antlr.ErrorListener;
import org.colomoto.biolqm.io.antlr.CNetParser;
import org.colomoto.biolqm.io.antlr.CNetLexer;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariableFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class CNetImport extends BaseLoader {

    public static Pattern PATTERN_commentID = Pattern.compile("#+\\s*([a-zA-Z_][a-zA-Z0-9_]*)");

    @Override
    protected LogicalModel performTask() throws Exception {

        CharStream input = new ANTLRInputStream(streams.reader());
        CommonTokenStream tokens = new CommonTokenStream( new CNetLexer(input) );
        CNetParser parser = new CNetParser( tokens );
        ErrorListener errors = new ErrorListener();
        parser.addErrorListener(errors);
        CNetParser.ModelContext mtx = parser.model();

        // Create boolean components with a default name
        int nbvar = Integer.parseInt( mtx.count().getText() );
        List<NodeInfo> components = new ArrayList<>(nbvar);
        for (int idx=0 ; idx<nbvar ; idx++) {
            // TODO: handle an optional multivalued case ?
            components.add( new NodeInfo("G"+idx));
        }

        // Prepare the MDD manager
        MDDVariableFactory mvf = new MDDVariableFactory();
        int max = 5;
        for (NodeInfo ni: components) {
            byte curmax = ni.getMax();
            if (curmax > max) {
                max = curmax;
            }
            mvf.add(ni, (byte)(curmax+1));
        }
        MDDManager ddmanager = MDDManagerFactory.getManager(mvf, max+1);

        // Load all functions
        int[] functions = new int[nbvar];
        for (CNetParser.TableContext ttx: mtx.table()) {
            int curComponent = getNodeIndex( ttx.curvar().getText());

            // Try to retrieve the node ID from the last comment before the table
            List<Token> hidden = tokens.getHiddenTokensToLeft( ttx.getStart().getTokenIndex(), 2);
            if (hidden != null) {
                String comment = hidden.get(hidden.size()-1).getText().trim();
                Matcher m = PATTERN_commentID.matcher(comment);
                if (m.matches()) {
                    components.get(curComponent).setNodeID(m.group(1));
                }
            }
            int count = Integer.parseInt( ttx.count().getText() );
            int[] regulators = new int[count];
            int i = 0;
            for (CNetParser.VaridContext vtx: ttx.varid()) {
                regulators[i++] = getNodeIndex(vtx.getText());
            }

            List<CNetParser.LineContext> ttlines = ttx.line();
            List<byte[]> states = new ArrayList<>( ttlines.size());
            for (CNetParser.LineContext ltx: ttlines) {
                String line = ltx.ttline().getText().trim();
                if (line.length() != (count+1)) {
                    throw new RuntimeException("Wrong number of values in truth table line: "+line.length() +" expected "+ (count+1));
                }

                // TODO: handle multi-valued
                int targetValue = Character.getNumericValue( line.charAt(count) );
                if (targetValue < 0 || targetValue > 1) {
                    throw new RuntimeException("Invalid target value");
                }
                if (targetValue == 0) {
                    continue;
                }

                byte[] vals = new byte[count];
                for (int idx=0 ; idx<count ; idx++) {
                    vals[idx] = (byte)Character.getNumericValue(line.charAt(idx));
                }

                byte[] state = new byte[nbvar];
                for (int k=0 ; k<state.length ; k++) {
                    state[k] = -1;
                }
                for (int k=0 ; k<regulators.length ; k++) {
                    state[ regulators[k] ] = vals[k];
                }
                states.add( state );
            }

            // Save the full function
            // TODO: handle multi-valued?
            int f = ddmanager.nodeFromStates(states, 1);
            functions[curComponent] = f;
        }

        // Assemble the complete model
        return new LogicalModelImpl(components, ddmanager, functions);
    }


    private int getNodeIndex(String s) {
        return Integer.parseInt(s) - 1;
    }
}
