package jcl.compiler.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.ThrowStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ThrowExpander extends MacroFunctionExpander<ThrowStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.THROW;
	}

	@Override
	public ThrowStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 3, "THROW");

		final ListStruct formRest = form.getRest();

		final LispStruct catchTag = formRest.getCar();
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct resultForm = formRestRest.getCar();
		final LispStruct resultFormAnalyzed = formAnalyzer.analyze(resultForm, environment);

		return new ThrowStruct(catchTagAnalyzed, resultFormAnalyzed);
	}
}
