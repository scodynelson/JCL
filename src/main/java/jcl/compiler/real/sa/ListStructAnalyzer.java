package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.real.sa.specialoperator.compiler.FunctionMarkerAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class ListStructAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ListStructAnalyzer INSTANCE = new ListStructAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		final LispStruct firstElement = input.getFirst();
		if (firstElement instanceof SymbolStruct) {
			final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, SemanticAnalyzer.environmentStack.peek());
			final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

			if (expandedForm.equals(NullStruct.INSTANCE)) {
				return NullStruct.INSTANCE;
			}

			if (expandedForm instanceof ListStruct) {
				final ListStruct expandedFormList = (ListStruct) expandedForm;
				if (expandedFormList.getFirst() instanceof SpecialOperator) {
					return SpecialOperatorAnalyzer.INSTANCE.analyze(expandedFormList);
				} else {
					return FunctionMarkerAnalyzer.INSTANCE.analyze(expandedFormList);
				}
			} else {
				return SemanticAnalyzer.saMainLoop(expandedForm);
			}
		} else if (firstElement instanceof ListStruct) {
			// ex ((lambda (x) (+ x 1)) 3)
			final ListStruct firstElementList = (ListStruct) firstElement;
			if (firstElementList.getFirst().equals(SpecialOperator.LAMBDA)) {
				input.setElement(1, LambdaAnalyzer.INSTANCE.analyze(firstElementList));
				return FunctionMarkerAnalyzer.INSTANCE.analyze(input);
			} else {
				throw new RuntimeException("Improperly Formed ListStruct: " + input);
			}
		} else {
			throw new RuntimeException("Improperly Formed ListStruct: " + input);
		}
	}
}
