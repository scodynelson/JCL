package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class FletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FletAnalyzer INSTANCE = new FletAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		final List<LispStruct> mungedFunctions = new ArrayList<>();
		mungedFunctions.add(SpecialOperator.PROGN);

		final ListStruct fletFunctionList = input.getRest();
		final List<LispStruct> fletFunctionJavaList = fletFunctionList.getAsJavaList();

		SemanticAnalyzer.getFunctionNames("FLET", input, fletFunctionJavaList);

		for (final LispStruct currentFunction : fletFunctionJavaList) {
			final ListStruct functionListStruct = (ListStruct) currentFunction;

			final LispStruct functionFirst = functionListStruct.getFirst();
			final SymbolStruct<?> functionName = (SymbolStruct) functionFirst;
			final SymbolStruct<?> gensymFunctionName = GensymFunction.funcall(functionName.getName());

			final LispStruct functionSecond = functionListStruct.getRest().getFirst();
			final ListStruct lambdaList = (ListStruct) functionSecond;
			final ListStruct body = functionListStruct.getRest().getRest();

			final List<LispStruct> mungedFunction = new ArrayList<>();

			final List<LispStruct> setSymbolFunction = new ArrayList<>();
			setSymbolFunction.add(GlobalPackageStruct.COMMON_LISP.findSymbol("SET-SYMBOL-FUNCTION").getSymbolStruct());
			setSymbolFunction.add(gensymFunctionName);

			final List<LispStruct> innerFunction = new ArrayList<>();
			innerFunction.add(SpecialOperator.FUNCTION);

			final List<LispStruct> innerLambda = new ArrayList<>();
			innerLambda.add(SpecialOperator.LAMBDA);
			innerLambda.add(lambdaList);

			final List<LispStruct> innerBlock = new ArrayList<>();
			innerBlock.add(SpecialOperator.BLOCK);
			innerBlock.add(gensymFunctionName);
			innerBlock.add(body);

			final ListStruct innerBlockListStruct = ListStruct.buildProperList(innerBlock);
			innerLambda.add(innerBlockListStruct);

			final ListStruct innerLambdaListStruct = ListStruct.buildProperList(innerLambda);
			innerFunction.add(innerLambdaListStruct);

			final ListStruct innerFunctionListStruct = ListStruct.buildProperList(innerFunction);
			setSymbolFunction.add(innerFunctionListStruct);

			final ListStruct setSymbolFunctionListStruct = ListStruct.buildProperList(setSymbolFunction);
			mungedFunction.add(setSymbolFunctionListStruct);

			final ListStruct mungedFunctionListStruct = ListStruct.buildProperList(mungedFunction);
			mungedFunctions.add(mungedFunctionListStruct);
		}

		final ListStruct mungedFunctionsListStruct = ListStruct.buildProperList(mungedFunctions);
		return PrognAnalyzer.INSTANCE.analyze(mungedFunctionsListStruct);
	}
}
