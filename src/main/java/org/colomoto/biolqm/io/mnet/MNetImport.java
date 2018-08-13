package org.colomoto.biolqm.io.mnet;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseLoader;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.biolqm.io.antlr.*;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.logicalfunction.FunctionNode;
import org.colomoto.mddlib.logicalfunction.OperandFactory;
import org.colomoto.mddlib.logicalfunction.SimpleOperandFactory;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Simple import reading multi-valued logical functions from a text file.
 * 
 * @author Aurelien Naldi
 */
public class MNetImport extends BaseLoader {


	public MNetImport(StreamProvider streams) {
		super(streams);
	}

	public LogicalModel performTask() throws IOException {

		CharStream input = new ANTLRInputStream(streams.reader());
		ErrorListener errors = new ErrorListener();
		MNetParser parser = getParser(input, errors);
		MNetParser.ModelContext mctx = parser.model();

		if (errors.hasErrors()) {
			// TODO: better check for errors
			System.err.println("Found some errors:");
			for (String s: errors.getErrors()) {
				System.err.println("  "+s);
			}
			return null;
		}

		Map<String, NodeInfo> id2var = new HashMap<String, NodeInfo>();
		List<NodeInfo> variables = new ArrayList<NodeInfo>();

		// first collect all valid variables
		for (MNetParser.AssignContext actx: mctx.assign()) {

			String id = actx.var().ID().getText();
			TerminalNode tnode = actx.var().VALUE();
			byte max = 1;
			if (tnode != null) {
				max = Byte.parseByte(tnode.getText());
				if (max < 1) {
					max = 1;
				}
			}

			if ( id2var.containsKey(id)) {
				NodeInfo ni = id2var.get(id);
				if (max > ni.getMax()) {
					ni.setMax(max);
				}
				continue;
			}

			NodeInfo ni = new NodeInfo(id, (byte)max);
			id2var.put(id, ni);
			variables.add( ni);
		}

		// Prepare the corresponding MDD manager
		MDDManager ddmanager;
		MDDVariableFactory mvf = new MDDVariableFactory();
		for (NodeInfo ni: variables) {
			mvf.add(ni, (byte)(ni.getMax()+1));
		}
		ddmanager = MDDManagerFactory.getManager(mvf, 10);
		MDDVariable[] ddvariables = ddmanager.getAllVariables();
		int[] functions = new int[variables.size()];


		// create the operand factory to assist the parser
		OperandFactory operandFactory = new SimpleOperandFactory<NodeInfo>(variables);
		MNetParserListener listener = new MNetParserListener(operandFactory);

		// then load the actual functions
		Map<NodeInfo,List<Assignment>> var2assign = new HashMap<>();
		for (MNetParser.AssignContext actx: mctx.assign()) {

			MNetParser.VarContext vc = actx.var();
			String id = vc.ID().getText();
			int th = 1;
			TerminalNode nth = vc.VALUE();
			if (nth != null) {
				th = Integer.parseInt(nth.getText());
			}
			NodeInfo ni = id2var.get(id);
			FunctionNode node = listener.loadExpr(actx.expr());

			// Add it to the list of assignments
			List<Assignment> asgs = var2assign.get(ni);
			if (asgs == null) {
				asgs = new ArrayList<>();
				var2assign.put(ni, asgs);
			}
			asgs.add( new Assignment(th, node));
		}


		return ExpressionStack.constructMVModel(operandFactory, variables, var2assign);
	}

	private MNetParser getParser(CharStream input, ErrorListener errors) {
		MNetLexer lexer = new MNetLexer(input);
		TokenStream tokens = new CommonTokenStream(lexer);
		MNetParser parser = new MNetParser(tokens);

		parser.removeErrorListeners();
		parser.addErrorListener(errors);

		return parser;
	}

}

class MNetParserListener extends MNetBaseListener {

	private final ParseTreeWalker walker = new ParseTreeWalker();
	private final ExpressionStack stack;

	public MNetParserListener( OperandFactory operandFactory) {
		this.stack = new ExpressionStack( operandFactory);
	}

	public FunctionNode loadExpr( ParseTree expr) {
		stack.clear();
		walker.walk(this, expr);
		return stack.done();
	}

	@Override
	public void exitVar(@NotNull MNetParser.VarContext ctx) {
		String var = ctx.ID().getText();
		if (ctx.VALUE() != null) {
			int v = Integer.parseInt( ctx.VALUE().getText());
			stack.ident(var, v);
		} else {
			stack.ident(var);
		}
	}

	@Override
	public void exitVal(@NotNull MNetParser.ValContext ctx) {
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
	public void exitAndExpr(@NotNull MNetParser.AndExprContext ctx) {
		stack.operator( Operator.AND);
	}

	@Override
	public void exitOrExpr(@NotNull MNetParser.OrExprContext ctx) {
		stack.operator( Operator.OR);
	}

	@Override
	public void exitSimpleExpr(@NotNull MNetParser.SimpleExprContext ctx) {
		List<?> nots = ctx.not();
		if (nots != null && nots.size() % 2 > 0) {
			stack.not();
		}
	}

}
