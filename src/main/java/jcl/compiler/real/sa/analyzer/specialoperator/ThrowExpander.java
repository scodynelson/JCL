package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ThrowExpander extends MacroFunctionExpander {

	private static final long serialVersionUID = 359191567361134081L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.THROW.setMacroFunctionExpander(this);
	}

	@Override
	public ThrowStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final int inputSize = form.size();
		if (inputSize != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + inputSize + ". Expected 3 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct catchTag = inputRest.getFirst();
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, analysisBuilder);

		final ListStruct inputRestRest = inputRest.getRest();

		final LispStruct resultForm = inputRestRest.getFirst();
		final LispStruct resultFormAnalyzed = formAnalyzer.analyze(resultForm, analysisBuilder);

		return new ThrowStruct(catchTagAnalyzed, resultFormAnalyzed);
	}
}
