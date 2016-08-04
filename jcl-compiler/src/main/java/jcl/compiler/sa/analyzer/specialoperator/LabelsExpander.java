package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.Stack;

import jcl.compiler.StackUtils;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.lang.SymbolStructImpl;
import jcl.lang.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LabelsExpander extends InnerLambdaExpander {

	@Autowired
	private FunctionExpander functionExpander;

	protected LabelsExpander() {
		super("LABELS");
	}

	@Override
	public SymbolStructImpl getFunctionSymbol() {
		return SpecialOperatorStructImpl.LABELS;
	}

	@Override
	protected InnerLambdaStruct buildInnerLambda(final ListStruct innerLambdas,
	                                             final Environment innerLambdaEnvironment,
	                                             final BodyProcessingResult bodyProcessingResult,
	                                             final DeclareStruct declare,
	                                             final Stack<SymbolStructImpl> functionNameStack,
	                                             final List<SymbolStructImpl> functionNames) {

		try {
			// Add function names BEFORE analyzing the functions.
			StackUtils.pushAll(functionNameStack, functionNames);

			final List<InnerLambdaStruct.InnerLambdaVar> vars
					= getVars(innerLambdas, innerLambdaEnvironment, declare, functionNames);
			return getInnerLambda(vars, innerLambdaEnvironment, bodyProcessingResult, declare);
		} finally {
			StackUtils.popX(functionNameStack, functionNames.size());
		}
	}

	@Override
	protected ListStruct getInnerLambdaBody(final ListStruct innerBlockListStruct, final SymbolStructImpl functionNameSymbol,
	                                        final List<SymbolStructImpl> functionNames) {
		return innerBlockListStruct;
	}

	@Override
	protected CompilerFunctionStruct expandBuiltInnerFunction(final ListStruct innerFunctionListStruct, final Environment environment) {
		// Evaluate in the 'current' environment.
		return functionExpander.expand(innerFunctionListStruct, environment);
	}
}
