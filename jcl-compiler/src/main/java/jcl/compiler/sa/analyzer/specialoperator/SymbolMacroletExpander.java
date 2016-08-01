package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.SymbolMacroBinding;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.SymbolMacroletStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.ListStruct;
import jcl.type.TType;
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
			throw new TypeErrorException("SYMBOL-MACROLET: PARAMETER-LIST must be a List. Got: " + first);
		}
		final ListStruct parameters = (ListStruct) first;

		final Environment symbolMacroletEnvironment = new Environment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = LispStructFactory.toProperList(bodyProcessingResult.getDeclares());
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
			throw new ProgramErrorException("SYMBOL-MACROLET: PARAMETER must be a List. Got: " + parameter);
		}

		final ListStruct listParameter = (ListStruct) parameter;
		final Iterator<LispStruct> iterator = listParameter.iterator();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must have only 2 elements. Got: 0");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: First element of parameter list must be a symbol. Got: " + first);
		}
		final SymbolStruct var = (SymbolStruct) first;

		final boolean hasGlobalBinding = Environment.NULL.hasDynamicBinding(var);
		if (hasGlobalBinding) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list symbol must not be a dynamic binding in the global environment. Got: " + var);
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
