package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.real.sa.specialoperator.compiler.FunctionMarkerAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

public class ListStructAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ListStructAnalyzer INSTANCE = new ListStructAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.equals(NullStruct.INSTANCE)) {
			return input;
		}

		final LispStruct first = input.getFirst();
		if (first instanceof SymbolStruct) {
			final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, SemanticAnalyzer.environmentStack.peek());
			final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

			if (expandedForm.equals(NullStruct.INSTANCE)) {
				return NullStruct.INSTANCE;
			}

			if (expandedForm instanceof ListStruct) {
				final ListStruct expandedFormList = (ListStruct) expandedForm;

				final LispStruct expandedFormListFirst = expandedFormList.getFirst();
				if (expandedFormListFirst instanceof SpecialOperator) {
					return SpecialOperatorAnalyzer.INSTANCE.analyze(expandedFormList);
				} else {
					return FunctionMarkerAnalyzer.INSTANCE.analyze(expandedFormList);
				}
			} else {
				return SemanticAnalyzer.saMainLoop(expandedForm);
			}
		} else if (first instanceof ListStruct) {
			// ex ((lambda (x) (+ x 1)) 3)
			final ListStruct firstAsList = (ListStruct) first;

			final LispStruct firstOfFirstList = firstAsList.getFirst();
			if (firstOfFirstList.equals(SpecialOperator.LAMBDA)) {
				final ListStruct lambdaAnalyzed = LambdaAnalyzer.INSTANCE.analyze(firstAsList);
				final ListStruct lambdaList = new ConsStruct(lambdaAnalyzed, input.getRest());

				return FunctionMarkerAnalyzer.INSTANCE.analyze(lambdaList);
			} else {
				throw new RuntimeException("SA LIST: First element of a first element ListStruct must be the SpecialOperator 'LAMBDA'. Got: " + firstOfFirstList);
			}
		} else {
			throw new RuntimeException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}
	}
}
