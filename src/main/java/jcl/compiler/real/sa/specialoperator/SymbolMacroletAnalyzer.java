package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.declaration.DeclareElement;
import jcl.compiler.real.sa.element.declaration.SpecialDeclarationElement;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class SymbolMacroletAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

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
			final LispStruct analyzedParameterForm = analyzer.analyzeForm(parameterForm, analysisBuilder);

			final ListStruct analyzedParameterListStruct = ListStruct.buildProperList(parameterName, analyzedParameterForm);
			analyzedParameterList.add(analyzedParameterListStruct);
		}

		final ListStruct analyzedParameterLL = ListStruct.buildProperList(analyzedParameterList);
		symbolMacroletResultList.add(analyzedParameterLL);

		final ListStruct body = input.getRest().getRest();
		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(analyzer, body, analysisBuilder);
		validateDeclares(bodyProcessingResult);

//		symbolMacroletResultList.addAll(bodyProcessingResult.getDeclarations()); // TODO: do we add these here really???
		symbolMacroletResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(symbolMacroletResultList);
	}

	private static void validateDeclares(final BodyProcessingResult bodyProcessingResult) {
		final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();
		if (declareElement != null) {
			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			if (!specialDeclarationElements.isEmpty()) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Special declarations not allowed. Got: " + specialDeclarationElements);
			}
		}
	}
}
