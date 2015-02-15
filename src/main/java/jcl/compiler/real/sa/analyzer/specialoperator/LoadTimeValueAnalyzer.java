package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.specialoperator.ImmutableLoadTimeValueElement;
import jcl.compiler.real.element.specialoperator.LoadTimeValueElement;
import jcl.compiler.real.element.specialoperator.MutableLoadTimeValueElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;
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

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
//		final Environment currentEnvironment = environmentStack.peek();
		final Environment currentLexicalEnvironment = environmentStack.getCurrentLexicalEnvironment();
		final Environment currentEnclosingLambda = getEnclosingLambda(currentLexicalEnvironment);

		final Environment nullLexicalEnvironment = Environment.NULL;
		environmentStack.push(nullLexicalEnvironment);

		try {
			final Element analyzedEvalForm = analyzer.analyzeForm(evalForm, analysisBuilder);

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
			environmentStack.pop();
		}
	}

	private static Environment getEnclosingLambda(final Environment lexicalEnvironment) {

		Environment currentLexicalEnvironment = lexicalEnvironment;

		final Marker marker = currentLexicalEnvironment.getMarker();
		while (!Marker.LAMBDA_MARKERS.contains(marker)) {
			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}
}
