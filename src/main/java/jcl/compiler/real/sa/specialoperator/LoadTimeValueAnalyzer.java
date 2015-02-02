package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.LoadTimeValueElement;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.BooleanStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Stack;
import java.util.UUID;

@Component
public class LoadTimeValueAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Override
	public LoadTimeValueElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if ((input.size() < 2) || (input.size() > 3)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: " + input.size() + ". Expected either 2 or 3 arguments.");
		}

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!(third instanceof BooleanStruct)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be of type BooleanStruct. Got: " + third);
		}

		final BooleanStruct readOnlyP = (BooleanStruct) third;
		final boolean isReadOnly = readOnlyP.booleanValue();

		// (eval form)
		// if read-only-p
		// return value
		// else
		// if in LTV, retrieve. else, store into lexical environment LTV list
		// return LTV value

		// TODO: what we need to do here is:
		// TODO: 1.) Get global instance of 'EVAL' function
		// TODO: 2.) Pass the 'form' to the 'EVAL' function
		// TODO: 3.) Forcefully evaluate the 'EVAL' function

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();
		final LexicalEnvironment currentEnclosingLambda = getEnclosingLambda(currentLexicalEnvironment);

		final String name = "LOAD_TIME_VALUE" + UUID.randomUUID();

		final List<LoadTimeValue> currentLoadTimeValues = currentEnclosingLambda.getLoadTimeValues();
		if (isReadOnly) {
		} else {
			final LoadTimeValue newLoadTimeValue = new LoadTimeValue(name, null);
			currentLoadTimeValues.add(newLoadTimeValue);
		}

		return new LoadTimeValueElement(name, null);
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

	/*
(def-ir1-translator load-time-value ((form &optional read-only-p) start cont)
  "Arrange for FORM to be evaluated at load-time and use the value produced
   as if it were a constant.  If READ-ONLY-P is non-NIL, then the resultant
   object is guaranteed to never be modified, so it can be put in read-only
   storage."
  (let ((value (handler-case (eval form)
	                         (error (condition)
		                            (compiler-error _N"(during EVAL of LOAD-TIME-VALUE)~%~A" condition)))))
	(ir1-convert start cont
		         (if read-only-p
			         `',value
			       `(value-cell-ref ',(make-value-cell value))))))
	 */
}
