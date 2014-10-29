package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
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
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final ListStruct parameterList = (ListStruct) second;
		final List<LispStruct> parameterJavaList = parameterList.getAsJavaList();

		for (final LispStruct currentParameter : parameterJavaList) {
			if (!(currentParameter instanceof ListStruct)) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be of type ListStruct. Got: " + second);
			}

			final ListStruct currentParameterList = (ListStruct) currentParameter;
			if (currentParameterList.size() != 2) {
				throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter must have only 2 elements. Got: " + currentParameter);
			}

			final LispStruct currentParameterListFirst = currentParameterList.getFirst();
			if (!(currentParameterListFirst instanceof SymbolStruct)) {
				throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + currentParameter);
			}
		}

		// TODO: Handle declarations that happen before the body!!!
		// TODO: don't allow :special declares...

		final List<LispStruct> symbolMacroletResultList = new ArrayList<>();
		symbolMacroletResultList.add(SpecialOperator.SYMBOL_MACROLET);
		symbolMacroletResultList.add(second);

		final ListStruct symbolMacroletBody = input.getRest().getRest();
		final List<LispStruct> symbolMacroletBodyJavaList = symbolMacroletBody.getAsJavaList();
		for (final LispStruct bodyForm : symbolMacroletBodyJavaList) {
			final LispStruct saResult = SemanticAnalyzer.saMainLoop(bodyForm);
			symbolMacroletResultList.add(saResult);
		}

		return ListStruct.buildProperList(symbolMacroletResultList);
	}
}
