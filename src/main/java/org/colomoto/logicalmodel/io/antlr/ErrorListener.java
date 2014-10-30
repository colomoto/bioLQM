package org.colomoto.logicalmodel.io.antlr;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.misc.Nullable;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

/**
 * A simple error listener to be shared among antlr-based parsers
 *
 * @author Aurelien Naldi
 */
public class ErrorListener extends BaseErrorListener {

    private final List<String> errors = new ArrayList<String>();

    public boolean hasErrors() {
        return errors.size() > 0;
    }

    @Override
    public void syntaxError(@NotNull Recognizer<?, ?> recognizer, @Nullable Object offendingSymbol, int line, int charPositionInLine, @NotNull String msg, @Nullable RecognitionException e) {
        errors.add(line+":"+charPositionInLine + "  "+msg);
    }

    @Override
    public void reportAmbiguity(@NotNull Parser recognizer, @NotNull DFA dfa, int startIndex, int stopIndex, boolean exact, @Nullable BitSet ambigAlts, @NotNull ATNConfigSet configs) {
        errors.add("Ambiguous");
    }

    @Override
    public void reportAttemptingFullContext(@NotNull Parser recognizer, @NotNull DFA dfa, int startIndex, int stopIndex, @Nullable BitSet conflictingAlts, @NotNull ATNConfigSet configs) {
        errors.add("Attempt Full Context");
    }

    @Override
    public void reportContextSensitivity(@NotNull Parser recognizer, @NotNull DFA dfa, int startIndex, int stopIndex, int prediction, @NotNull ATNConfigSet configs) {
        errors.add("Context-sensitive");
    }
}
