package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.MacroFunctionBinding;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

public class FunctionAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FunctionAnalyzer INSTANCE = new FunctionAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {
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
				final MacroFunctionBinding macroFunctionBinding = (MacroFunctionBinding) fnBinding.getBinding(functionSymbol);
				final SymbolStruct<?> functionBindingName = macroFunctionBinding.getName();

				final LispStruct first = input.getFirst();
				return new ConsStruct(first, functionBindingName);
			}
		} else if (second instanceof ListStruct) {
			final ListStruct functionList = (ListStruct) second;

			final LispStruct functionListFirst = functionList.getFirst();

			if (functionListFirst.equals(SpecialOperator.LAMBDA)) {
				final Environment newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LAMBDA);
				final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();
				EnvironmentAccessor.createParent(newEnvironment, parentEnvironment);
				SemanticAnalyzer.environmentStack.push(newEnvironment);

				final int tempPosition = SemanticAnalyzer.bindingsPosition;
				try {
					return LambdaAnalyzer.INSTANCE.analyze(functionList);
				} finally {
					SemanticAnalyzer.bindingsPosition = tempPosition;
					SemanticAnalyzer.environmentStack.pop();
					SemanticAnalyzer.currentLispName.pop();
				}
			} else if (functionListFirst.equals(SpecialOperator.MACRO_LAMBDA)) {
				final Environment newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.MACRO);
				final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();
				EnvironmentAccessor.createParent(newEnvironment, parentEnvironment);
				SemanticAnalyzer.environmentStack.push(newEnvironment);

				final int tempPosition = SemanticAnalyzer.bindingsPosition;
				try {
					return LambdaAnalyzer.INSTANCE.analyze(functionList);
				} finally {
					SemanticAnalyzer.bindingsPosition = tempPosition;
					SemanticAnalyzer.environmentStack.pop();
					SemanticAnalyzer.currentLispName.pop();
				}
			}

			if (functionListFirst.equals(GlobalPackageStruct.COMMON_LISP.findSymbol("SETF").getSymbolStruct())) {
				return input; // no changes at this level
			}

			throw new RuntimeException("Improperly Formed Function: if the first argument is a ListStruct, it must be a LAMBDA");
		}

		throw new RuntimeException("Improperly Formed Function: the arguments must be either a ListStruct or SymbolStruct");
	}
}
