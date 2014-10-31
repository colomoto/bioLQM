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
import org.colomoto.mddlib.logicalfunction.FunctionNode;
import org.colomoto.mddlib.logicalfunction.OperandFactory;

import java.io.IOException;
import java.io.Reader;
import java.util.List;

/**
 * launch an antlr-generated parser, listen for events and feed them to an ExpressionStack
 *
 * @author Aurelien Naldi
 */
public class BooleanNetParserListener extends BooleanNetBaseListener {

    private final ParseTreeWalker walker = new ParseTreeWalker();
    private final ErrorListener errorListener = new ErrorListener();

    private final OperandFactory operandFactory;
    private final ExpressionStack stack;

    public BooleanNetParserListener( OperandFactory operandFactory) {

        this.operandFactory = operandFactory;
        this.stack = new ExpressionStack(operandFactory);

    }

    private BooleanNetParser getParser(CharStream input) {
        BooleanNetLexer lexer = new BooleanNetLexer(input);
        TokenStream tokens = new CommonTokenStream(lexer);
        BooleanNetParser parser = new BooleanNetParser(tokens);

        parser.removeErrorListeners();
        parser.addErrorListener(errorListener);

        return parser;
    }

    public FunctionNode getExpr( String e) {
        return getExpr( new ANTLRInputStream( e));
    }

    public FunctionNode getExpr( Reader reader) throws IOException {
        return getExpr( new ANTLRInputStream(reader));
    }

    public FunctionNode getExpr( CharStream input) {
        BooleanNetParser parser = getParser( input);
        ParseTree tree = parser.expr();

        if (errorListener.hasErrors()) {
            System.out.println("Found some errors");
            return null;
        }
        return loadExpr( tree);
    }

    private FunctionNode loadExpr( ParseTree expr) {
        stack.clear();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(this, expr);

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
    public void exitAndExpr(@NotNull BooleanNetParser.AndExprContext ctx) {
        stack.operator( Operator.AND);
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
    public void exitAssign(@NotNull BooleanNetParser.AssignContext ctx) {
        ctx.var();
        ParseTree tree = ctx.expr();
        FunctionNode node = loadExpr( tree);
    }
}
