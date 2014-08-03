package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class SymbolMacroletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final SymbolMacroletAnalyzer INSTANCE = new SymbolMacroletAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new RuntimeException("SYMBOL-MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final ListStruct parameterList = (ListStruct) second;
		final List<LispStruct> parameterJavaList = parameterList.getAsJavaList();

		for (final LispStruct currentParameter : parameterJavaList) {
			if (!(currentParameter instanceof ListStruct)) {
				throw new RuntimeException("SYMBOL-MACROLET: Parameter must be of type ListStruct. Got: " + second);
			}

			final ListStruct currentParameterList = (ListStruct) currentParameter;
			if (currentParameterList.size() != 2) {
				throw new RuntimeException("SYMBOL-MACROLET: ListStruct parameter must have only 2 elements. Got: " + currentParameter);
			}

			final LispStruct currentParameterListFirst = currentParameterList.getFirst();
			if (!(currentParameterListFirst instanceof SymbolStruct)) {
				throw new RuntimeException("SYMBOL-MACROLET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + currentParameter);
			}
		}

		// TODO: Handle declarations that happen before the body!!!
		final ListStruct symbolMacroletBody = input.getRest().getRest();
		final ListStruct prognResults = PrognAnalyzer.INSTANCE.analyze(symbolMacroletBody);
		final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

		final List<LispStruct> symbolMacroletResultList = new ArrayList<>();
		symbolMacroletResultList.add(SpecialOperator.SYMBOL_MACROLET);
		symbolMacroletResultList.add(second);
		symbolMacroletResultList.addAll(javaPrognResults);

		return ListStruct.buildProperList(symbolMacroletResultList);
	}
}
