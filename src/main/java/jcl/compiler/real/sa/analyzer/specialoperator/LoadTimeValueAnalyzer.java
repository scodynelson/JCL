package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.ImmutableLoadTimeValueElement;
import jcl.compiler.real.sa.element.LoadTimeValueElement;
import jcl.compiler.real.sa.element.MutableLoadTimeValueElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Stack;
import java.util.UUID;

@Component
public class LoadTimeValueAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2168018740373766746L;

	@Override
	public LoadTimeValueElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if ((inputSize < 2) || (inputSize > 3)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: " + inputSize + ". Expected either 2 or 3 arguments.");
		}

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!(third instanceof BooleanStruct)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be of type BooleanStruct. Got: " + third);
		}

		final BooleanStruct readOnlyP = (BooleanStruct) third;
		final boolean isReadOnly = readOnlyP.booleanValue();

		final LispStruct form = input.getRest().getFirst();

		final SymbolStruct<?> evalFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("EVAL").getSymbolStruct();
		final ListStruct evalForm = new ConsStruct(evalFnSym, form);

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();
		final LexicalEnvironment currentEnclosingLambda = getEnclosingLambda(currentLexicalEnvironment);

		final LexicalEnvironment nullLexicalEnvironment = LexicalEnvironment.NULL;
		lexicalEnvironmentStack.push(nullLexicalEnvironment);

		try {
			final LispStruct analyzedEvalForm = analyzer.analyzeForm(evalForm, analysisBuilder);

			if (isReadOnly) {
				final UUID uniqueLTVId = UUID.randomUUID();

				final List<LoadTimeValue> currentLoadTimeValues = currentEnclosingLambda.getLoadTimeValues();

				final LoadTimeValue newLoadTimeValue = new LoadTimeValue(uniqueLTVId, analyzedEvalForm);
				currentLoadTimeValues.add(newLoadTimeValue);

				return new ImmutableLoadTimeValueElement(uniqueLTVId);
			} else {
				return new MutableLoadTimeValueElement(analyzedEvalForm);
			}
		} finally {
			lexicalEnvironmentStack.pop();
		}
	}

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param lexicalEnvironment
	 * 		The environment that is enclosed by a lambda
	 *
	 * @return The lambda enclosing the given environment.
	 */
	private static LexicalEnvironment getEnclosingLambda(final LexicalEnvironment lexicalEnvironment) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		final Marker marker = currentLexicalEnvironment.getMarker();
		while (!Marker.LAMBDA_MARKERS.contains(marker)) {
			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}
}
