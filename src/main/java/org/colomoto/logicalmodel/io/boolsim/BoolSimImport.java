package org.colomoto.logicalmodel.io.boolsim;

import java.io.IOException;
import java.io.Reader;
import java.util.*;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.io.antlr.*;
import org.colomoto.mddlib.logicalfunction.FunctionNode;
import org.colomoto.mddlib.logicalfunction.OperandFactory;
import org.colomoto.mddlib.logicalfunction.SimpleOperandFactory;
import org.colomoto.mddlib.logicalfunction.operators.AndOperatorFactory;
import org.colomoto.mddlib.logicalfunction.operators.NotOperatorFactory;
import org.colomoto.mddlib.logicalfunction.operators.OrOperatorFactory;

/**
 * Import boolsim models.
 * 
 * @author Aurelien Naldi
 * @author Julien Dorier
 */
public class BoolSimImport {

	/**
	 * Entry point to parse a full model.
	 *
	 * @return
	 */
	public static LogicalModel getModel( Reader reader) throws IOException {

		CharStream input = new ANTLRInputStream(reader);
		ErrorListener errors = new ErrorListener();
		BoolsimParser parser = getParser(input, errors);
		BoolsimParser.ModelContext mctx = parser.model();

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
		for (BoolsimParser.AssignContext actx: mctx.assign()) {
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
		BoolsimParserListener listener = new BoolsimParserListener( operandFactory);

		// then load the actual functions
		Map<NodeInfo, FunctionNode[]> var2functions = new HashMap<NodeInfo, FunctionNode[]>();
		for (BoolsimParser.AssignContext actx: mctx.assign()) {
			String id = actx.var().ID().getText();
			int signIdx = 0;
			if (actx.op().POSITIVE() == null) {
				signIdx = 1;
			}

			NodeInfo ni = id2var.get( id);
			FunctionNode node = listener.loadExpr(actx.expr());

			FunctionNode[] functions = var2functions.get( ni);
			if (functions == null) {
				functions = new FunctionNode[2];
				var2functions.put(ni, functions);
			}

			FunctionNode curNode = functions[signIdx];
			if (curNode != null) {
				node = OrOperatorFactory.FACTORY.getNode(curNode, node);
			}
			functions[signIdx] = node;
		}

		// integrate positive and negative effects
		Map<NodeInfo, FunctionNode> var2function = new HashMap<NodeInfo, FunctionNode>();
		for (NodeInfo ni: var2functions.keySet()) {
			FunctionNode[] functions = var2functions.get(ni);
			FunctionNode posNode = functions[0];
			FunctionNode negNode = functions[1];
			if (negNode != null) {
				negNode = NotOperatorFactory.FACTORY.getNode( negNode);

				if (posNode != null) {
					negNode = AndOperatorFactory.FACTORY.getNode(posNode, negNode);
				}
				var2function.put(ni, negNode);
			} else {
				var2function.put(ni, posNode);
			}
		}

		return ExpressionStack.constructModel(operandFactory, variables, var2function);
	}


	private static BoolsimParser getParser(CharStream input, ErrorListener errors) {
		BoolsimLexer lexer = new BoolsimLexer(input);
		TokenStream tokens = new CommonTokenStream(lexer);
		BoolsimParser parser = new BoolsimParser(tokens);

		parser.removeErrorListeners();
		parser.addErrorListener(errors);

		return parser;
	}

}


class BoolsimParserListener extends BoolsimBaseListener {

	private final ParseTreeWalker walker = new ParseTreeWalker();
	private final ExpressionStack stack;

	public BoolsimParserListener( OperandFactory operandFactory) {
		this.stack = new ExpressionStack( operandFactory);
	}

	public FunctionNode loadExpr( ParseTree expr) {
		stack.clear();
		walker.walk(this, expr);

		return stack.done();
	}

	@Override
	public void exitVar(@NotNull BoolsimParser.VarContext ctx) {
		String var = ctx.ID().getText();
		stack.ident(var);
	}

	@Override
	public void exitAndExpr(@NotNull BoolsimParser.AndExprContext ctx) {
		stack.operator( Operator.AND);
	}

	@Override
	public void exitSimpleExpr(@NotNull BoolsimParser.SimpleExprContext ctx) {
		List nots = ctx.not();
		if (nots != null && nots.size() % 2 > 0) {
			stack.not();
		}
	}

}
