package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.LispFormValueValidator;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ThrowExpander extends MacroFunctionExpander<ThrowStruct> {

	private static final long serialVersionUID = 359191567361134081L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.THROW;
	}

	@Override
	public ThrowStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 3, "THROW");

		final ListStruct formRest = form.getRest();

		final LispStruct catchTag = formRest.getFirst();
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct resultForm = formRestRest.getFirst();
		final LispStruct resultFormAnalyzed = formAnalyzer.analyze(resultForm, environment);

		return new ThrowStruct(catchTagAnalyzed, resultFormAnalyzed);
	}
}
