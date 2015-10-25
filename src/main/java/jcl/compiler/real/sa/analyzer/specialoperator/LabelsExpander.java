package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.StackUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LabelsExpander extends InnerLambdaExpander {

	private static final long serialVersionUID = -3698985413039911540L;

	@Autowired
	private FunctionExpander functionExpander;

	protected LabelsExpander() {
		super("LABELS");
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.LABELS;
	}

	@Override
	protected InnerLambdaStruct buildInnerLambda(final List<LispStruct> innerLambdasAsJavaList,
	                                             final Environment innerLambdaEnvironment,
	                                             final BodyProcessingResult bodyProcessingResult,
	                                             final DeclareStruct declare,
	                                             final Stack<SymbolStruct<?>> functionNameStack,
	                                             final List<SymbolStruct<?>> functionNames) {

		try {
			// Add function names BEFORE analyzing the functions.
			StackUtils.pushAll(functionNameStack, functionNames);

			final List<InnerLambdaStruct.InnerLambdaVar> vars
					= getVars(innerLambdasAsJavaList, innerLambdaEnvironment, declare, functionNames);
			return getInnerLambda(vars, innerLambdaEnvironment, bodyProcessingResult, declare);
		} finally {
			StackUtils.popX(functionNameStack, functionNames.size());
		}
	}

	@Override
	protected ListStruct getInnerLambdaBody(final ListStruct innerBlockListStruct, final SymbolStruct<?> functionNameSymbol,
	                                        final List<SymbolStruct<?>> functionNames) {
		return innerBlockListStruct;
	}

	@Override
	protected CompilerFunctionStruct expandBuiltInnerFunction(final ListStruct innerFunctionListStruct, final Environment environment) {
		// Evaluate in the 'current' environment.
		return functionExpander.expand(innerFunctionListStruct, environment);
	}
}
