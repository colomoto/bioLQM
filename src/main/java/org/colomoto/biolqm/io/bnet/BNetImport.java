package org.colomoto.biolqm.io.bnet;

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
 * @author Hannes Klarner (minor modification to file by Aurelien Naldi)
 */
public class BNetImport {

	/**
	 * Entry point to parse a full model.
	 *
	 * @param reader source from which to parse
	 * @return the reconstructed model
	 * @throws IOException if reading fails
	 */
	public static LogicalModel getModel( Reader reader) throws IOException {

		CharStream input = new ANTLRInputStream(reader);
		ErrorListener errors = new ErrorListener();
		BNetParser parser = getParser(input, errors);
		BNetParser.ModelContext mctx = parser.model();

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
		for (BNetParser.AssignContext actx: mctx.assign()) {

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
		BNetParserListener listener = new BNetParserListener(operandFactory);
		
		// System.out.println("HANNES: "+variables);

		// then load the actual functions
		Map<NodeInfo, FunctionNode> var2function = new HashMap<NodeInfo, FunctionNode>();
		for (BNetParser.AssignContext actx: mctx.assign()) {
		   
			String id = actx.var().ID().getText();
			
			NodeInfo ni = id2var.get(id);
			FunctionNode node = listener.loadExpr(actx.expr());
			FunctionNode curNode = var2function.get(ni);
			
			if (curNode != null) {
				node = OrOperatorFactory.FACTORY.getNode(curNode, node);
			}
			var2function.put( ni, node);
		}

		return ExpressionStack.constructModel(operandFactory, variables, var2function);
	}

	private static BNetParser getParser(CharStream input, ErrorListener errors) {
		BNetLexer lexer = new BNetLexer(input);
		TokenStream tokens = new CommonTokenStream(lexer);
		BNetParser parser = new BNetParser(tokens);

		parser.removeErrorListeners();
		parser.addErrorListener(errors);

		return parser;
	}
}


class BNetParserListener extends BNetBaseListener {

	private final ParseTreeWalker walker = new ParseTreeWalker();
	private final ExpressionStack stack;

	public BNetParserListener( OperandFactory operandFactory) {
		this.stack = new ExpressionStack( operandFactory);
	}

	public FunctionNode loadExpr( ParseTree expr) {
		stack.clear();
		walker.walk(this, expr);

		return stack.done();
	}

	@Override
	public void exitVar(@NotNull BNetParser.VarContext ctx) {
		String var = ctx.ID().getText();
		stack.ident(var);
	}

	@Override
	public void exitVal(@NotNull BNetParser.ValContext ctx) {
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
	public void exitAndExpr(@NotNull BNetParser.AndExprContext ctx) {
		stack.operator( Operator.AND);
	}

	@Override
	public void exitOrExpr(@NotNull BNetParser.OrExprContext ctx) {
		stack.operator( Operator.OR);
	}

	@Override
	public void exitSimpleExpr(@NotNull BNetParser.SimpleExprContext ctx) {
		List<?> nots = ctx.not();
		if (nots != null && nots.size() % 2 > 0) {
			stack.not();
		}
	}

}
