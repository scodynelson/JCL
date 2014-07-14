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

public class MacroletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MacroletAnalyzer INSTANCE = new MacroletAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		// TODO: right now this acts just like FLET. Need to fix this once we can macroexpand correctly...

		final List<LispStruct> mungedMacros = new ArrayList<>();
		mungedMacros.add(SpecialOperator.PROGN);

		final ListStruct macroletMacroList = input.getRest();
		final List<LispStruct> macroletMacroJavaList = macroletMacroList.getAsJavaList();

		SemanticAnalyzer.getFunctionNames("MACROLET", input, macroletMacroJavaList);

		for (final LispStruct currentMacro : macroletMacroJavaList) {
			final ListStruct macroListStruct = (ListStruct) currentMacro;

			final LispStruct macroFirst = macroListStruct.getFirst();
			final SymbolStruct<?> macroName = (SymbolStruct) macroFirst;
			final SymbolStruct<?> gensymMacroName = GensymFunction.funcall(macroName.getName());

			final LispStruct macroSecond = macroListStruct.getRest().getFirst();
			final ListStruct lambdaList = (ListStruct) macroSecond;
			final ListStruct body = macroListStruct.getRest().getRest();

			final List<LispStruct> mungedMacro = new ArrayList<>();

			final List<LispStruct> setSymbolFunction = new ArrayList<>();
			setSymbolFunction.add(GlobalPackageStruct.COMMON_LISP.findSymbol("SET-SYMBOL-FUNCTION").getSymbolStruct());
			setSymbolFunction.add(gensymMacroName);

			final List<LispStruct> innerFunction = new ArrayList<>();
			innerFunction.add(SpecialOperator.FUNCTION);

			final List<LispStruct> innerLambda = new ArrayList<>();
			innerLambda.add(SpecialOperator.LAMBDA);
			innerLambda.add(lambdaList);

			final List<LispStruct> innerBlock = new ArrayList<>();
			innerBlock.add(SpecialOperator.BLOCK);
			innerBlock.add(gensymMacroName);
			innerBlock.add(body);

			final ListStruct innerBlockListStruct = ListStruct.buildProperList(innerBlock);
			innerLambda.add(innerBlockListStruct);

			final ListStruct innerLambdaListStruct = ListStruct.buildProperList(innerLambda);
			innerFunction.add(innerLambdaListStruct);

			final ListStruct innerFunctionListStruct = ListStruct.buildProperList(innerFunction);
			setSymbolFunction.add(innerFunctionListStruct);

			final ListStruct setSymbolFunctionListStruct = ListStruct.buildProperList(setSymbolFunction);
			mungedMacro.add(setSymbolFunctionListStruct);

			final ListStruct mungedFunctionListStruct = ListStruct.buildProperList(mungedMacro);
			mungedMacros.add(mungedFunctionListStruct);
		}

		final ListStruct mungedMacrosListStruct = ListStruct.buildProperList(mungedMacros);
		return PrognAnalyzer.INSTANCE.analyze(mungedMacrosListStruct);
	}
}
