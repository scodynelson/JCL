package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.Stack;

import jcl.compiler.StackUtils;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public final class LabelsExpander extends InnerLambdaExpander {

	public static final LabelsExpander INSTANCE = new LabelsExpander();

	private LabelsExpander() {
		super("LABELS");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.LABELS;
	}

	@Override
	protected InnerLambdaStruct buildInnerLambda(final ListStruct innerLambdas,
	                                             final Environment innerLambdaEnvironment,
	                                             final BodyProcessingResult bodyProcessingResult,
	                                             final DeclareStruct declare,
	                                             final Stack<SymbolStruct> functionNameStack,
	                                             final List<SymbolStruct> functionNames) {

		try {
			// Add function names BEFORE analyzing the functions.
			StackUtils.pushAll(functionNameStack, functionNames);

			final List<InnerLambdaStruct.InnerLambdaVar> vars
					= getVars(innerLambdas, innerLambdaEnvironment, declare, functionNames);
			return getInnerLambda(vars, innerLambdaEnvironment, bodyProcessingResult);
		} finally {
			StackUtils.popX(functionNameStack, functionNames.size());
		}
	}

	@Override
	protected ListStruct getInnerLambdaBody(final ListStruct innerBlockListStruct, final SymbolStruct functionNameSymbol,
	                                        final List<SymbolStruct> functionNames) {
		return innerBlockListStruct;
	}

	@Override
	protected CompilerFunctionStruct expandBuiltInnerFunction(final ListStruct innerFunctionListStruct, final Environment environment) {
		// Evaluate in the 'current' environment.
		return FunctionExpander.INSTANCE.expand(innerFunctionListStruct, environment);
	}
}
