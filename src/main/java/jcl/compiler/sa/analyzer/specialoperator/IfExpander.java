package jcl.compiler.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.IfStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfExpander extends MacroFunctionExpander<IfStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.IF;
	}

	@Override
	public IfStruct expand(final ListStruct form, final Environment environment) {
		final int formSize = validator.validateListFormSize(form, 3, 4, "IF");

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
