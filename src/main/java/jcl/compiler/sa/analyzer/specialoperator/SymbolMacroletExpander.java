package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

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
import jcl.conditions.exceptions.TypeErrorException;
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

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.SYMBOL_MACROLET;
	}

	@Override
	public SymbolMacroletStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // SYMBOL-MACROLET SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof ListStruct)) {
			final String printedObject = printer.print(first);
			throw new TypeErrorException("SYMBOL-MACROLET: PARAMETER-LIST must be a List. Got: " + printedObject);
		}
		final ListStruct parameters = (ListStruct) first;

		final Environment symbolMacroletEnvironment = new Environment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, symbolMacroletEnvironment);
		validateDeclares(declare);

		final List<SymbolMacroletStruct.SymbolMacroletVar> symbolMacroletVars
				= parameters.stream()
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
			throw new ProgramErrorException("SYMBOL-MACROLET: PARAMETER must be a List. Got: " + printedParameter);
		}

		final ListStruct listParameter = (ListStruct) parameter;
		final Iterator<LispStruct> iterator = listParameter.iterator();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must have only 2 elements. Got: 0");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			final String printedObject = printer.print(first);
			throw new ProgramErrorException("SYMBOL-MACROLET: First element of parameter list must be a symbol. Got: " + printedObject);
		}
		final SymbolStruct var = (SymbolStruct) first;

		final boolean hasGlobalBinding = Environment.NULL.hasDynamicBinding(var);
		if (hasGlobalBinding) {
			final String printedObject = printer.print(var);
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list symbol must not be a dynamic binding in the global environment. Got: " + printedObject);
		}

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must have only 2 elements. Got: 1");
		}
		final LispStruct parameterValue = iterator.next();

		if (iterator.hasNext()) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must have only 2 elements. Got: 3");
		}

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = symbolMacroletEnvironment.getParent();
		final LispStruct expansion = formAnalyzer.analyze(parameterValue, parentEnvironment);

		final SymbolMacroBinding binding = new SymbolMacroBinding(var, TType.INSTANCE, expansion);
		symbolMacroletEnvironment.addSymbolMacroBinding(binding);

		return new SymbolMacroletStruct.SymbolMacroletVar(var, expansion);
	}
}
