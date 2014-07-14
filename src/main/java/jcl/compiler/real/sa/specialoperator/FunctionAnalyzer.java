package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class FunctionAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FunctionAnalyzer INSTANCE = new FunctionAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		if (input.size() != 2) {
			throw new RuntimeException("Wrong number of arguments to special operator Function: " + input.size());
		}

		final LispStruct second = input.getRest().getFirst();
		if (second instanceof SymbolStruct) {
			final SymbolStruct<?> functionSymbol = (SymbolStruct) second;
			final Environment fnBinding = EnvironmentAccessor.getBindingEnvironment(SemanticAnalyzer.environmentStack.peek(), functionSymbol, false);

			if (fnBinding.equals(Environment.NULL)) {
				SymbolStructAnalyzer.INSTANCE.analyze(functionSymbol);
				return input;
			} else {
				final FunctionBinding functionBinding = (FunctionBinding) fnBinding.getBinding(functionSymbol);
				final SymbolStruct<?> functionBindingName = functionBinding.getName();

				final LispStruct first = input.getFirst();
				return new ConsStruct(first, functionBindingName);
			}
		} else if (second instanceof ListStruct) {
			final ListStruct functionList = (ListStruct) second;

			final LispStruct functionListFirst = functionList.getFirst();
			if (functionListFirst.equals(SpecialOperator.LAMBDA) || functionListFirst.equals(SpecialOperator.MACRO_LAMBDA)) {
				return SemanticAnalyzer.saLambdaAux(functionList);
			}

			if (functionListFirst.equals(GlobalPackageStruct.COMMON_LISP.findSymbol("SETF").getSymbolStruct())) {
				return input; // no changes at this level
			}

			throw new RuntimeException("Improperly Formed Function: if the first argument is a ListStruct, it must be a LAMBDA");
		}

		throw new RuntimeException("Improperly Formed Function: the arguments must be either a ListStruct or SymbolStruct");
	}
}
