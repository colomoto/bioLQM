package org.colomoto.logicalmodel.io.functions;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.colomoto.logicalmodel.io.antlr.ErrorListener;
import org.colomoto.logicalmodel.io.antlr.ExpressionStack;
import org.colomoto.logicalmodel.io.antlr.Operator;
import org.colomoto.logicalmodel.io.antlr.Value;
import org.colomoto.logicalmodel.io.antlr.BooleanFunctionBaseListener;
import org.colomoto.logicalmodel.io.antlr.BooleanFunctionLexer;
import org.colomoto.logicalmodel.io.antlr.BooleanFunctionParser;

import java.util.List;

/**
 * launch an antlr-generated parser, listen for events and feed them to an ExpressionStack
 *
 * @author Aurelien Naldi
 */
public class BooleanFunctionParserListener extends BooleanFunctionBaseListener {

    ExpressionStack stack;

    public String parse(String e) {

        ErrorListener errorListener = new ErrorListener();

        // parser for a given string
        CharStream input = new ANTLRInputStream(e);
        BooleanFunctionLexer lexer = new BooleanFunctionLexer( input);
        TokenStream tokens = new CommonTokenStream(lexer);
        BooleanFunctionParser parser = new BooleanFunctionParser(tokens);

        parser.removeErrorListeners();
        parser.addErrorListener( errorListener);

        // actual parsing target
        ParseTree tree = parser.expr();

        if (errorListener.hasErrors()) {
            System.out.println("Found some errors");
            return null;
        }

        this.stack = new ExpressionStack();
        new ParseTreeWalker().walk(this, tree);

        return stack.done();
    }

    @Override
    public void exitVar(@NotNull BooleanFunctionParser.VarContext ctx) {
        String var = ctx.ID().getText();
        stack.ident(var);
    }

    @Override
    public void exitVal(@NotNull BooleanFunctionParser.ValContext ctx) {
        String s = ctx.VALUE().getText().trim();

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
    public void exitOrExpr(@NotNull BooleanFunctionParser.OrExprContext ctx) {
        stack.operator( Operator.OR);
    }

    @Override
    public void exitSimpleExpr(@NotNull BooleanFunctionParser.SimpleExprContext ctx) {
        List nots = ctx.not();
        if (nots != null && nots.size() % 2 > 0) {
            stack.not();
        }
    }

    @Override
    public void exitAndExpr(@NotNull BooleanFunctionParser.AndExprContext ctx) {
        stack.operator( Operator.AND);
    }
}
