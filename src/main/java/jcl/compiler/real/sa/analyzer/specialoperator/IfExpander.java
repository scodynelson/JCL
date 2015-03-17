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

		final int formSize = form.size();
		if ((formSize < 3) || (formSize > 4)) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: " + formSize + ". Expected either 3 or 4 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct testForm = formRest.getFirst();
		final LispStruct testFormAnalyzed = formAnalyzer.analyze(testForm, environment);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct thenForm = formRestRest.getFirst();
		final LispStruct thenFormAnalyzed = formAnalyzer.analyze(thenForm, environment);

		final LispStruct elseFormAnalyzed;
		if (formSize == 4) {
			final ListStruct formRestRestRest = formRestRest.getRest();

			final LispStruct elseForm = formRestRestRest.getFirst();
			elseFormAnalyzed = formAnalyzer.analyze(elseForm, environment);
		} else {
			elseFormAnalyzed = NullStruct.INSTANCE;
		}

		return new IfStruct(testFormAnalyzed, thenFormAnalyzed, elseFormAnalyzed);
	}
}
