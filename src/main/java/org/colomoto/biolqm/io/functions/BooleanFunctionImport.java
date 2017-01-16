package org.colomoto.biolqm.io.functions;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.antlr.*;
import org.colomoto.mddlib.logicalfunction.FunctionNode;
import org.colomoto.mddlib.logicalfunction.OperandFactory;
import org.colomoto.mddlib.logicalfunction.SimpleOperandFactory;
import org.colomoto.mddlib.logicalfunction.operators.OrOperatorFactory;

/**
 * Simple import reading logical functions in a text file.
 * 
 * @author Aurelien Naldi
 */
public class BooleanFunctionImport {

	/**
	 * Entry point to parse a full model.
	 *
	 * @return
	 */
	public static LogicalModel getModel( Reader reader) throws IOException {

		CharStream input = new ANTLRInputStream(reader);
		ErrorListener errors = new ErrorListener();
		BooleanFunctionParser parser = getParser(input, errors);
		BooleanFunctionParser.ModelContext mctx = parser.model();

		if (errors.hasErrors()) {
			// TODO: better check for errors
			System.out.println("Found some errors:");
			for (String s: errors.getErrors()) {
				System.out.println("  "+s);
			}
			return null;
		}

		Map<String, NodeInfo> id2var = new HashMap<String, NodeInfo>();
		List<NodeInfo> variables = new ArrayList<NodeInfo>();

		// first collect all valid variables
		for (BooleanFunctionParser.AssignContext actx: mctx.assign()) {
			String id = actx.var().ID().getText();
			if ( id2var.containsKey(id)) {
				continue;
			}

			NodeInfo ni = new NodeInfo(id);
			id2var.put(id, ni);
			variables.add( ni);
		}

		// create the operand factory to assist the parser
		OperandFactory operandFactory = new SimpleOperandFactory<NodeInfo>(variables);
		BooleanFunctionParserListener listener = new BooleanFunctionParserListener( operandFactory);

		// then load the actual functions
		Map<NodeInfo, FunctionNode> var2function = new HashMap<NodeInfo, FunctionNode>();
		for (BooleanFunctionParser.AssignContext actx: mctx.assign()) {
			String id = actx.var().ID().getText();
			NodeInfo ni = id2var.get( id);
			FunctionNode node = listener.loadExpr(actx.expr());

			FunctionNode curNode = var2function.get( ni);
			if (curNode != null) {
				node = OrOperatorFactory.FACTORY.getNode(curNode, node);
			}
			var2function.put( ni, node);
		}

		return ExpressionStack.constructModel(operandFactory, variables, var2function);
	}

	/**
	 * Entry point to parse a single expression
	 *
	 * @param factory
	 * @param e
	 * @return
	 */
	public static FunctionNode getExpr( OperandFactory factory, String e) {
		CharStream input = new ANTLRInputStream( e);
		ErrorListener errors = new ErrorListener();
		BooleanFunctionParser parser = getParser(input, errors);
		ParseTree tree = parser.expr();

		if (errors.hasErrors()) {
			System.out.println("Errors!!");
			for (String s: errors.getErrors()) {
				System.out.println("  "+s);
			}
			return null;
		}
		BooleanFunctionParserListener listener = new BooleanFunctionParserListener( factory);
		return listener.loadExpr(tree);
	}


	private static BooleanFunctionParser getParser(CharStream input, ErrorListener errors) {
		BooleanFunctionLexer lexer = new BooleanFunctionLexer(input);
		TokenStream tokens = new CommonTokenStream(lexer);
		BooleanFunctionParser parser = new BooleanFunctionParser(tokens);

		parser.removeErrorListeners();
		parser.addErrorListener(errors);

		return parser;
	}
}


class BooleanFunctionParserListener extends BooleanFunctionBaseListener {

	private final ParseTreeWalker walker = new ParseTreeWalker();
	private final ExpressionStack stack;

	public BooleanFunctionParserListener( OperandFactory operandFactory) {
		this.stack = new ExpressionStack( operandFactory);
	}

	public FunctionNode loadExpr( ParseTree expr) {
		stack.clear();
		walker.walk(this, expr);

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
	public void exitAndExpr(@NotNull BooleanFunctionParser.AndExprContext ctx) {
		stack.operator( Operator.AND);
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

}
