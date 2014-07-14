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
import java.util.Map;

public class LabelsAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LabelsAnalyzer INSTANCE = new LabelsAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		final List<LispStruct> mungedFunctions = new ArrayList<>();
		mungedFunctions.add(SpecialOperator.PROGN);

		final ListStruct labelsFunctionList = input.getRest();
		final List<LispStruct> labelsFunctionJavaList = labelsFunctionList.getAsJavaList();

		final Map<SymbolStruct<?>, SymbolStruct<?>> functionNameMap = SemanticAnalyzer.getFunctionNames("LABELS", input, labelsFunctionJavaList);

		for (final LispStruct currentFunction : labelsFunctionJavaList) {
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

			final ListStruct labelsFunctionBody = getLabelsFunctionBody(body, functionNameMap);
			innerBlock.add(labelsFunctionBody);

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

	private static ListStruct getLabelsFunctionBody(final ListStruct body, final Map<SymbolStruct<?>, SymbolStruct<?>> functionNameMap) {

		final List<LispStruct> newBody = new ArrayList<>();

		final List<LispStruct> bodyJavaList = body.getAsJavaList();
		for (final LispStruct bodyElement : bodyJavaList) {
			if (bodyElement instanceof ListStruct) {
				final ListStruct bodyElementList = (ListStruct) bodyElement;
				final ListStruct updatedElement = getLabelsFunctionBody(bodyElementList, functionNameMap);
				newBody.add(updatedElement);
			} else if (bodyElement instanceof SymbolStruct) {
				final SymbolStruct<?> bodyElementSymbol = (SymbolStruct) bodyElement;
				if (functionNameMap.containsKey(bodyElementSymbol)) {
					newBody.add(functionNameMap.get(bodyElementSymbol));
				} else {
					newBody.add(bodyElementSymbol);
				}
			} else {
				newBody.add(bodyElement);
			}
		}

		return ListStruct.buildProperList(newBody);
	}

}
