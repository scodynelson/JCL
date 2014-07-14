package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class MultipleValueProg1Analyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MultipleValueProg1Analyzer INSTANCE = new MultipleValueProg1Analyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		if (input.size() < 2) {
			throw new RuntimeException("Wrong number of arguments to special operator MULTIPLE-VALUE-PROG1: " + input.size());
		}

		final List<LispStruct> letBlock = new ArrayList<>();
		letBlock.add(SpecialOperator.LET);

		final List<LispStruct> allParameters = new ArrayList<>();

		final List<LispStruct> parameter = new ArrayList<>();
		final SymbolStruct<?> tempParamName = GensymFunction.funcall();
		parameter.add(tempParamName);

		final List<LispStruct> multipleValueListPart = new ArrayList<>();
		multipleValueListPart.add(GlobalPackageStruct.COMMON_LISP.findSymbol("MULTIPLE-VALUE-LIST").getSymbolStruct());
		final LispStruct firstForm = input.getRest().getFirst();
		multipleValueListPart.add(firstForm);
		final ListStruct multipleValueListPartList = ListStruct.buildProperList(multipleValueListPart);

/*
(defmacro multiple-value-list (&rest forms)
  (declare (system::%java-class-name "lisp.common.function.MultipleValueList"))
  `(multiple-value-call (function list) ,@forms))
 */

		parameter.add(multipleValueListPartList);
		final ListStruct parameterList = ListStruct.buildProperList(parameter);

		allParameters.add(parameterList);
		final ListStruct allParametersList = ListStruct.buildProperList(allParameters);

		letBlock.add(allParametersList);

		final List<LispStruct> prognPart = new ArrayList<>();
		prognPart.add(SpecialOperator.PROGN);
		prognPart.add(input.getRest().getRest());

		final List<LispStruct> valuesListPart = new ArrayList<>();
		valuesListPart.add(GlobalPackageStruct.COMMON_LISP.findSymbol("VALUES-LIST").getSymbolStruct());
		valuesListPart.add(ListStruct.buildProperList(tempParamName));
		final ListStruct valuesListPartList = ListStruct.buildProperList(valuesListPart);

		prognPart.add(valuesListPartList);
		final ListStruct prognPartList = ListStruct.buildProperList(prognPart);

		letBlock.add(prognPartList);
		final ListStruct letBlockList = ListStruct.buildProperList(letBlock);

		return LetAnalyzer.INSTANCE.analyze(letBlockList);
	}
}
