package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.lambdalist.AuxBinding;
import jcl.compiler.real.environment.lambdalist.KeyBinding;
import jcl.compiler.real.environment.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.LambdaEnvironmentListStruct;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.BodyProcessingUtil;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class LambdaAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LambdaAnalyzer INSTANCE = new LambdaAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LAMBDA: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = semanticAnalyzer.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final Environment lambdaEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LAMBDA);
		lambdaEnvironment.setParent(parentEnvironment);

		environmentStack.push(lambdaEnvironment);

		final int tempPosition = semanticAnalyzer.getBindingsPosition();
		try {
			final ListStruct parameters = (ListStruct) second;
			final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(semanticAnalyzer, parameters);

			final ListStruct currentBodyForms = input.getRest().getRest();
			final BodyProcessingUtil.BodyProcessingResult bodyProcessingResult = BodyProcessingUtil.processBodyWithDeclsAndDoc(semanticAnalyzer, currentBodyForms);

			final Environment envList = environmentStack.peek();

			final List<LispStruct> newStartingLambdaBody = getNewStartingLambdaBody(parsedLambdaList);
			newStartingLambdaBody.addAll(bodyProcessingResult.getBodyForms());

			final ListStruct newBodyForms = ListStruct.buildProperList(newStartingLambdaBody);
			return new LambdaEnvironmentListStruct(envList, bodyProcessingResult.getDeclarations(), newBodyForms, parsedLambdaList, bodyProcessingResult.getDocString());
		} finally {
			semanticAnalyzer.setBindingsPosition(tempPosition);
			environmentStack.pop();
		}
	}

	private static List<LispStruct> getNewStartingLambdaBody(final OrdinaryLambdaListBindings parsedLambdaList) {
		final List<LispStruct> newLambdaBody = new ArrayList<>();

		final List<AuxBinding> auxBindings = parsedLambdaList.getAuxBindings();
		if (auxBindings.isEmpty()) {
			newLambdaBody.add(SpecialOperator.PROGN);
		} else {
			newLambdaBody.add(SpecialOperator.LET_STAR);

			final List<LispStruct> auxLetStarVars = new ArrayList<>(auxBindings.size());
			for (final AuxBinding auxBinding : auxBindings) {
				final ConsStruct auxLetStarVar = new ConsStruct(auxBinding.getSymbolStruct(), auxBinding.getInitForm());
				auxLetStarVars.add(auxLetStarVar);
			}

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
}
