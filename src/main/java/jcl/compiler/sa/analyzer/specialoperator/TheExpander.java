package jcl.compiler.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.TheStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TheExpander extends MacroFunctionExpander<TheStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.THE;
	}

	@Override
	public TheStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSizeExact(form, 3, "THE");

		final ListStruct formRest = form.getRest();

		final LispStruct valueType = formRest.getFirst();
		validator.validateObjectTypes(valueType, "THE", "TYPE SPECIFIER", SymbolStruct.class, ListStruct.class);
		// TODO: do we actually want to somehow factor in the 'TypeSpecifier' produced by the second value?

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct theForm = formRestRest.getFirst();
		final LispStruct theFormAnalyzed = formAnalyzer.analyze(theForm, environment);

		return new TheStruct(null, theFormAnalyzed);
	}
}
