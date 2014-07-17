package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.specialoperator.FunctionAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

public class LambdaAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LambdaAnalyzer INSTANCE = new LambdaAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {
		// macroexpand the LAMBDA form, by default makes it (FUNCTION (LAMBDA... ))
		final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input);
		// now it has the right form, now handle it to saFunction

		ListStruct functionToAnalyze = (ListStruct) macroExpandReturn.getExpandedForm();
		if (!macroExpandReturn.wasExpanded()) {
			final ListStruct inter = ListStruct.buildProperList(functionToAnalyze);
			functionToAnalyze = new ConsStruct(SpecialOperator.FUNCTION, inter);
		}
		return FunctionAnalyzer.INSTANCE.analyze(functionToAnalyze);
	}
}
