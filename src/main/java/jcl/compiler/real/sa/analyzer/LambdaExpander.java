package jcl.compiler.real.sa.analyzer;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaExpander extends MacroFunctionExpander<LambdaStruct> {

	private static final long serialVersionUID = -7592502247452528911L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LAMBDA.setMacroFunctionExpander(this);
	}

	@Override
	public LambdaStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("LAMBDA: Parameter list must be a list. Got: " + printedObject);
		}

		final LambdaEnvironment lambdaEnvironment = new LambdaEnvironment(environment);

		final ListStruct parameters = (ListStruct) second;

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms, lambdaEnvironment);
		final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();

		final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		specialDeclarationElements.forEach(specialDeclarationElement -> Environments.addDynamicVariableBinding(specialDeclarationElement, lambdaEnvironment));

		final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(formAnalyzer, lambdaEnvironment, parameters, declareElement);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final ListStruct newStartingLambdaBody = getNewStartingLambdaBody(parsedLambdaList, bodyForms);
		final List<LispStruct> newLambdaBodyForms = newStartingLambdaBody.getAsJavaList();

		final List<LispStruct> analyzedBodyForms
				= newLambdaBodyForms.stream()
				                    .map(e -> formAnalyzer.analyze(e, lambdaEnvironment))
				                    .collect(Collectors.toList());
		return new LambdaStruct(parsedLambdaList, bodyProcessingResult.getDocString(), analyzedBodyForms, lambdaEnvironment);
	}

	private static ListStruct getNewStartingLambdaBody(final OrdinaryLambdaListBindings parsedLambdaList,
	                                                   final List<LispStruct> bodyForms) {

		final List<LispStruct> newLambdaBody = new ArrayList<>();

		final List<AuxBinding> auxBindings = parsedLambdaList.getAuxBindings();
		if (auxBindings.isEmpty()) {
			newLambdaBody.add(SpecialOperator.PROGN);
		} else {
			newLambdaBody.add(SpecialOperator.LET_STAR);

			final List<LispStruct> auxLetStarVars
					= auxBindings.stream()
					             .map(e -> ListStruct.buildProperList(e.getSymbolStruct(), e.getInitForm()))
					             .collect(Collectors.toList());

			final ListStruct auxLetStarParams = ListStruct.buildProperList(auxLetStarVars);
			newLambdaBody.add(auxLetStarParams);
		}

		final List<ListStruct> initFormIfSetqs = getInitFormIfSetqs(parsedLambdaList);
		newLambdaBody.addAll(initFormIfSetqs);

		newLambdaBody.addAll(bodyForms);

		final ListStruct newLambdaBodyList = ListStruct.buildProperList(newLambdaBody);
		// NOTE: We need to wrap this one more time so that we can use the analyzer to analyze the 'forms' correctly
		//       even though there will only be either a 'progn' or 'let*' form.
		return ListStruct.buildProperList(newLambdaBodyList);
	}

	private static List<ListStruct> getInitFormIfSetqs(final OrdinaryLambdaListBindings parsedLambdaList) {

		final List<ListStruct> initFormIfSetqs = new ArrayList<>();

		final List<OptionalBinding> optionalBindings = parsedLambdaList.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();

			final SymbolStruct<?> optionalVar = optionalBinding.getSymbolStruct();
			final LispStruct optionalInitForm = optionalBinding.getInitForm();
			final ListStruct initFormSetq
					= ListStruct.buildProperList(SpecialOperator.SETQ, optionalVar, optionalInitForm);

			final SymbolStruct<?> suppliedPVar = suppliedPBinding.getSymbolStruct();
			final ListStruct initFormIfSetq
					= ListStruct.buildProperList(SpecialOperator.IF, suppliedPVar, initFormSetq);

			initFormIfSetqs.add(initFormIfSetq);
		}

		final List<KeyBinding> keyBindings = parsedLambdaList.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();

			final SymbolStruct<?> keyVar = keyBinding.getSymbolStruct();
			final LispStruct keyInitForm = keyBinding.getInitForm();
			final ListStruct initFormSetq
					= ListStruct.buildProperList(SpecialOperator.SETQ, keyVar, keyInitForm);

			final SymbolStruct<?> suppliedPVar = suppliedPBinding.getSymbolStruct();
			final ListStruct initFormIfSetq
					= ListStruct.buildProperList(SpecialOperator.IF, suppliedPVar, initFormSetq);

			initFormIfSetqs.add(initFormIfSetq);
		}

		return initFormIfSetqs;
	}
}
