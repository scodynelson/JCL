package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ThrowExpander extends MacroFunctionExpander<ThrowStruct> {

	private static final long serialVersionUID = 359191567361134081L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the throw macro function and adds it to the special operator 'throw'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.THROW.setMacroFunctionExpander(this);
	}

	@Override
	public ThrowStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + formSize + ". Expected 3 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct catchTag = formRest.getFirst();
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct resultForm = formRestRest.getFirst();
		final LispStruct resultFormAnalyzed = formAnalyzer.analyze(resultForm, environment);

		return new ThrowStruct(catchTagAnalyzed, resultFormAnalyzed);
	}
}
