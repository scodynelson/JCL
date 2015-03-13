package jcl.compiler.real.sa.analyzer;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.EnhancedLinkedList;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaExpander extends MacroFunctionExpander<LambdaStruct> {

	private static final long serialVersionUID = -7592502247452528911L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LAMBDA.setMacroFunctionExpander(this);
	}

	@Override
	public LambdaStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LAMBDA: Parameter list must be of type ListStruct. Got: " + second);
		}

		final LambdaEnvironment lambdaEnvironment = new LambdaEnvironment(environment);

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> bodyForms = inputRest.getRest().getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(bodyForms, lambdaEnvironment);
		final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();

		final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(formAnalyzer, lambdaEnvironment, parameters, declareElement);
		final EnhancedLinkedList<LispStruct> newStartingLambdaBody = getNewStartingLambdaBody(parsedLambdaList);

		final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		specialDeclarationElements.forEach(e -> addDynamicVariableBinding(e, lambdaEnvironment));

		final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();
		newStartingLambdaBody.addAll(realBodyForms);

		final ListStruct newLambdaBodyListStruct = ListStruct.buildProperList(newStartingLambdaBody);

		final LispStruct analyzedBodyForms = formAnalyzer.analyze(newLambdaBodyListStruct, lambdaEnvironment);
		return new LambdaStruct(parsedLambdaList, bodyProcessingResult.getDocString(), analyzedBodyForms, lambdaEnvironment);
	}

	private static EnhancedLinkedList<LispStruct> getNewStartingLambdaBody(final OrdinaryLambdaListBindings parsedLambdaList) {
		final EnhancedLinkedList<LispStruct> newLambdaBody = new EnhancedLinkedList<>();

		final List<AuxBinding> auxBindings = parsedLambdaList.getAuxBindings();
		if (auxBindings.isEmpty()) {
			newLambdaBody.add(SpecialOperator.PROGN);
		} else {
			newLambdaBody.add(SpecialOperator.LET_STAR);

			final List<LispStruct> auxLetStarVars = auxBindings
					.stream()
					.map(e -> ListStruct.buildProperList(e.getSymbolStruct(), e.getInitForm()))
					.collect(Collectors.toList());

			final ListStruct auxLetStarVarsLL = ListStruct.buildProperList(auxLetStarVars);
			newLambdaBody.add(auxLetStarVarsLL);
		}

		final List<LispStruct> initFormIfSetqs = getInitFormIfSetqs(parsedLambdaList);
		newLambdaBody.addAll(initFormIfSetqs);

		return newLambdaBody;
	}

	private static List<LispStruct> getInitFormIfSetqs(final OrdinaryLambdaListBindings parsedLambdaList) {

		final List<LispStruct> initFormIfSetqs = new ArrayList<>();

		final List<OptionalBinding> optionalBindings = parsedLambdaList.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();

			final List<LispStruct> initFormIfSetq = new ArrayList<>();
			initFormIfSetq.add(SpecialOperator.IF);
			initFormIfSetq.add(suppliedPBinding.getSymbolStruct());

			final List<LispStruct> initFormSetq = new ArrayList<>();
			initFormSetq.add(SpecialOperator.SETQ);
			initFormSetq.add(optionalBinding.getSymbolStruct());
			initFormSetq.add(optionalBinding.getInitForm());

			final ListStruct initFormSetqLL = ListStruct.buildProperList(initFormSetq);
			initFormIfSetq.add(initFormSetqLL);

			final ListStruct initFormIfSetqLL = ListStruct.buildProperList(initFormIfSetq);
			initFormIfSetqs.add(initFormIfSetqLL);
		}

		final List<KeyBinding> keyBindings = parsedLambdaList.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();

			final List<LispStruct> initFormIfSetq = new ArrayList<>();
			initFormIfSetq.add(SpecialOperator.IF);
			initFormIfSetq.add(suppliedPBinding.getSymbolStruct());

			final List<LispStruct> initFormSetq = new ArrayList<>();
			initFormSetq.add(SpecialOperator.SETQ);
			initFormSetq.add(keyBinding.getSymbolStruct());
			initFormSetq.add(keyBinding.getInitForm());

			final ListStruct initFormSetqLL = ListStruct.buildProperList(initFormSetq);
			initFormIfSetq.add(initFormSetqLL);

			final ListStruct initFormIfSetqLL = ListStruct.buildProperList(initFormIfSetq);
			initFormIfSetqs.add(initFormIfSetqLL);
		}

		return initFormIfSetqs;
	}

	private static void addDynamicVariableBinding(final SpecialDeclarationStruct specialDeclarationElement,
	                                              final LambdaEnvironment lambdaEnvironment) {

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(lambdaEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		lambdaEnvironment.setBindingsPosition(newBindingsPosition);

		final SymbolStruct<?> var = specialDeclarationElement.getVar();

		final Environment bindingEnvironment = Environments.getDynamicBindingEnvironment(lambdaEnvironment, var);
		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);

		final EnvironmentEnvironmentBinding binding = new EnvironmentEnvironmentBinding(var, allocation, T.INSTANCE, bindingEnvironment);
		lambdaEnvironment.addDynamicBinding(binding);
	}
}
