package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfExpander extends MacroFunctionExpander<IfStruct> {

	private static final long serialVersionUID = -5414856145190749144L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the if macro function and adds it to the special operator 'if'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.IF.setMacroFunctionExpander(this);
	}

	@Override
	public IfStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if ((inputSize < 3) || (inputSize > 4)) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: " + inputSize + ". Expected either 3 or 4 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct testForm = inputRest.getFirst();
		final LispStruct testFormAnalyzed = formAnalyzer.analyze(testForm, environment);

		final ListStruct inputRestRest = inputRest.getRest();

		final LispStruct thenForm = inputRestRest.getFirst();
		final LispStruct thenFormAnalyzed = formAnalyzer.analyze(thenForm, environment);

		LispStruct elseFormAnalyzed = NullStruct.INSTANCE;
		if (inputSize == 4) {
			final ListStruct inputRestRestRest = inputRestRest.getRest();

			final LispStruct elseForm = inputRestRestRest.getFirst();
			elseFormAnalyzed = formAnalyzer.analyze(elseForm, environment);
		}

		return new IfStruct(testFormAnalyzed, thenFormAnalyzed, elseFormAnalyzed);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
