package org.colomoto.logicalmodel.io.booleannet;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.colomoto.logicalmodel.io.antlr.*;
import org.colomoto.logicalmodel.io.antlr.BooleanNetBaseListener;
import org.colomoto.logicalmodel.io.antlr.BooleanNetLexer;
import org.colomoto.logicalmodel.io.antlr.BooleanNetParser;

import java.util.List;

/**
 * launch an antlr-generated parser, listen for events and feed them to an ExpressionStack
 *
 * @author Aurelien Naldi
 */
public class BooleanNetParserListener extends BooleanNetBaseListener {

    private final ExpressionStack stack = new ExpressionStack();
    private final ParseTreeWalker walker = new ParseTreeWalker();

    public String parse(String e) {

        ErrorListener errorListener = new ErrorListener();

        // parser for a given string
        CharStream input = new ANTLRInputStream(e);
        BooleanNetLexer lexer = new BooleanNetLexer( input);
        TokenStream tokens = new CommonTokenStream(lexer);
        BooleanNetParser parser = new BooleanNetParser(tokens);

        parser.removeErrorListeners();
        parser.addErrorListener( errorListener);

        // actual parsing target
        ParseTree tree = parser.expr();

        if (errorListener.hasErrors()) {
            System.out.println("Found some errors");
            return null;
        }

        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(this, tree);

        return stack.done();
    }

    @Override
    public void exitVar(@NotNull BooleanNetParser.VarContext ctx) {
        String var = ctx.ID().getText();
        stack.ident(var);
    }

    @Override
    public void exitVal(@NotNull BooleanNetParser.ValContext ctx) {
        String s = ctx.VALUE().getText().trim();
        if ("true".equalsIgnoreCase(s)) {
            stack.value( Value.TRUE );
            return;
        }

        if ("false".equalsIgnoreCase(s)) {
            stack.value( Value.TRUE );
            return;
        }

        try {
            int v = Integer.parseInt( s);
            if (v > 0) {
                stack.value( Value.TRUE );
            } else {
                stack.value( Value.FALSE );
            }
            return;
        } catch (Exception e) {
            throw new RuntimeException("Invalid value");
        }
    }

    @Override
    public void exitOrExpr(@NotNull BooleanNetParser.OrExprContext ctx) {
        stack.operator( Operator.OR);
    }

    @Override
    public void exitSimpleExpr(@NotNull BooleanNetParser.SimpleExprContext ctx) {
        List nots = ctx.not();
        if (nots != null && nots.size() % 2 > 0) {
            stack.not();
        }
    }

    @Override
    public void exitAndExpr(@NotNull BooleanNetParser.AndExprContext ctx) {
        stack.operator( Operator.AND);
    }
}
