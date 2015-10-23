package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.LispFormValueValidator;
import jcl.compiler.real.struct.specialoperator.MultipleValueProg1Struct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueProg1Expander extends MacroFunctionExpander<MultipleValueProg1Struct> {

	private static final long serialVersionUID = 1791554561862006171L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.MULTIPLE_VALUE_PROG1;
	}

	@Override
	public MultipleValueProg1Struct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "MULTIPLE-VALUE-PROG1");

		final ListStruct formRest = form.getRest();

		final LispStruct firstForm = formRest.getFirst();
		final LispStruct firstFormAnalyzed = formAnalyzer.analyze(firstForm, environment);

		final ListStruct formRestRest = formRest.getRest();

		final List<LispStruct> forms = formRestRest.getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new MultipleValueProg1Struct(firstFormAnalyzed, new PrognStruct(analyzedForms));
	}
}
