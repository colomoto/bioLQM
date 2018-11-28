package org.colomoto.biolqm.io.truthtable;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseLoader;
import org.colomoto.biolqm.io.antlr.LTTLexer;
import org.colomoto.biolqm.io.antlr.LTTParser;
import org.colomoto.biolqm.io.antlr.ErrorListener;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariableFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class LogicalTruthTableImport extends BaseLoader {

    @Override
    protected LogicalModel performTask() throws Exception {

        CharStream input = new ANTLRInputStream(streams.reader());
        CommonTokenStream tokens = new CommonTokenStream( new LTTLexer(input) );
        LTTParser parser = new LTTParser( tokens );
        ErrorListener errors = new ErrorListener();
        parser.addErrorListener(errors);
        LTTParser.ModelContext mtx = parser.model();

        // Peak forward to create all components and prepare the MDD manager
        List<LTTParser.TableContext> tables = mtx.table();
        int nbvar = tables.size();
        List<NodeInfo> components = new ArrayList<>(nbvar);
        Map<String,Integer> id2index = new HashMap<>();
        MDDVariableFactory mvf = new MDDVariableFactory();
        int max = 5;
        for (int idx=0 ; idx<nbvar ; idx++) {
            LTTParser.TableContext ttx = tables.get(idx);
            String id = ttx.curvar().getText();
            id2index.put(id, idx);
            byte curmax = 1;
            try {
                curmax = Byte.parseByte( ttx.max().getText() );
            } catch (Exception e) {
                // no max was defined
            }
            NodeInfo ni = new NodeInfo(id, curmax);
            components.add(ni);
            if (curmax > max) {
                max = curmax;
            }
            mvf.add(ni, (byte)(curmax+1));
        }
        MDDManager ddmanager = MDDManagerFactory.getManager(mvf, max+1);


        // Load all functions
        int[] functions = new int[nbvar];
        for (LTTParser.TableContext ttx: mtx.table()) {
            int curComponent = id2index.get( ttx.curvar().getText() );
            List<LTTParser.VarContext> regulatorIDs = ttx.var();
            int[] regulators = new int[regulatorIDs.size()];
            int i = 0;
            for (LTTParser.VarContext vtx: regulatorIDs) {
                regulators[i++] = id2index.get(vtx.getText());
            }

            List<LTTParser.LineContext> ttlines = ttx.line();
            List<byte[]> states = new ArrayList<>( ttlines.size());
            int count = regulators.length;
            for (LTTParser.LineContext ltx: ttlines) {
                int targetValue = Integer.parseInt( ltx.target().getText() );
                if (targetValue == 0) {
                    continue;
                }

                String line = ltx.ttline().getText().trim();
                if (line.length() != count) {
                    throw new RuntimeException("Wrong number of values in truth table line: "+line.length() +" expected "+ count);
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
            // FIXME: handle multi-valued!
            int f = ddmanager.nodeFromStates(states, 1);
            functions[curComponent] = f;
        }

        // Assemble the complete model
        return new LogicalModelImpl(components, ddmanager, functions);
    }

}
