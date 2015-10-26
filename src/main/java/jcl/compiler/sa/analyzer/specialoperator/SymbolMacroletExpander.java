package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.SymbolMacroBinding;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.SymbolMacroletStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SymbolMacroletExpander extends MacroFunctionExpander<SymbolMacroletStruct> {

	private static final long serialVersionUID = 3878455475225336840L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the symbol-macrolet macro function and adds it to the special operator 'symbol-macrolet'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.SYMBOL_MACROLET.setMacroFunctionExpander(this);
	}

	@Override
	public SymbolMacroletStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must be of type ListStruct. Got: " + printedObject);
		}

		final Environment symbolMacroletEnvironment = new Environment(environment);

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, symbolMacroletEnvironment);
		validateDeclares(declare);

		final List<SymbolMacroletStruct.SymbolMacroletVar> symbolMacroletVars
				= parametersAsJavaList.stream()
				                      .map(e -> getSymbolMacroletElementVar(e, declare, symbolMacroletEnvironment))
				                      .collect(Collectors.toList());

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, symbolMacroletEnvironment))
				           .collect(Collectors.toList());

		return new SymbolMacroletStruct(symbolMacroletVars, new PrognStruct(analyzedBodyForms), symbolMacroletEnvironment);
	}

	private static void validateDeclares(final DeclareStruct declare) {
		if (declare != null) {
			final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
			if (!specialDeclarations.isEmpty()) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Special declarations not allowed.");
			}
		}
	}

	private SymbolMacroletStruct.SymbolMacroletVar getSymbolMacroletElementVar(final LispStruct parameter, final DeclareStruct declare,
	                                                                           final Environment symbolMacroletEnvironment) {

		if (!(parameter instanceof ListStruct)) {
			final String printedParameter = printer.print(parameter);
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be a list. Got: " + printedParameter);
		}

		final ListStruct listParameter = (ListStruct) parameter;
		final SymbolStruct<?> var = getSymbolMacroletParameterVar(listParameter);
		final LispStruct expansion = getSymbolMacroletParameterExpansion(listParameter, symbolMacroletEnvironment);

		final SymbolMacroBinding binding = new SymbolMacroBinding(var, TType.INSTANCE, expansion);
		symbolMacroletEnvironment.addSymbolMacroBinding(binding);

		return new SymbolMacroletStruct.SymbolMacroletVar(var, expansion);
	}

	private SymbolStruct<?> getSymbolMacroletParameterVar(final ListStruct listParameter) {

		final int listParameterSize = listParameter.size();
		if (listParameterSize != 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must have only 2 elements. Got: " + listParameter);
		}

		final LispStruct listParameterFirst = listParameter.getFirst();
		if (!(listParameterFirst instanceof SymbolStruct)) {
			final String printedObject = printer.print(listParameterFirst);
			throw new ProgramErrorException("SYMBOL-MACROLET: First element of parameter list must be a symbol. Got: " + printedObject);
		}

		final SymbolStruct<?> parameterVar = (SymbolStruct<?>) listParameterFirst;

		final boolean hasGlobalBinding = Environment.NULL.hasDynamicBinding(parameterVar);
		if (hasGlobalBinding) {
			final String printedObject = printer.print(parameterVar);
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list symbol must not be a dynamic binding in the global environment. Got: " + printedObject);
		}

		return parameterVar;
	}

	private LispStruct getSymbolMacroletParameterExpansion(final ListStruct listParameter, final Environment symbolMacroletEnvironment) {

		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = symbolMacroletEnvironment.getParent();
		return formAnalyzer.analyze(parameterValue, parentEnvironment);
	}
}
