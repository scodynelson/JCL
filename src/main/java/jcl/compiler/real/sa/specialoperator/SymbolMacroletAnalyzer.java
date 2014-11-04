package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.Declaration;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class SymbolMacroletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final SymbolMacroletAnalyzer INSTANCE = new SymbolMacroletAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final ListStruct parameterList = (ListStruct) second;
		final List<LispStruct> parameterJavaList = parameterList.getAsJavaList();

		final List<LispStruct> symbolMacroletResultList = new ArrayList<>();
		symbolMacroletResultList.add(SpecialOperator.SYMBOL_MACROLET);

		final List<LispStruct> analyzedParameterList = new ArrayList<>(parameterJavaList.size());

		for (final LispStruct parameter : parameterJavaList) {
			if (!(parameter instanceof ListStruct)) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be of type ListStruct. Got: " + second);
			}

			final ListStruct parameterListStruct = (ListStruct) parameter;
			if (parameterListStruct.size() != 2) {
				throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter must have only 2 elements. Got: " + parameter);
			}

			final LispStruct parameterName = parameterListStruct.getFirst();
			if (!(parameterName instanceof SymbolStruct)) {
				throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + parameter);
			}

			final LispStruct parameterForm = parameterListStruct.getRest().getFirst();
			final LispStruct analyzedParameterForm = analyzer.analyzeForm(parameterForm);

			final ListStruct analyzedParameterListStruct = ListStruct.buildProperList(parameterName, analyzedParameterForm);
			analyzedParameterList.add(analyzedParameterListStruct);
		}

		final ListStruct analyzedParameterLL = ListStruct.buildProperList(analyzedParameterList);
		symbolMacroletResultList.add(analyzedParameterLL);

		final ListStruct body = input.getRest().getRest();
		final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.INSTANCE.analyze(body, analyzer);
		validateDeclares(bodyProcessingResult);

		symbolMacroletResultList.addAll(bodyProcessingResult.getDeclarations()); // TODO: do we add these here really???
		symbolMacroletResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(symbolMacroletResultList);
	}

	private static void validateDeclares(final BodyProcessingResult bodyProcessingResult) {
		final List<ListStruct> declarations = bodyProcessingResult.getDeclarations();

		for (final ListStruct declaration : declarations) {

			final LispStruct declarationIdentifier = declaration.getFirst();
			if (Declaration.SPECIAL.equals(declarationIdentifier)) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Special declaration not allowed. Got: " + declaration);
			}
		}
	}
}
