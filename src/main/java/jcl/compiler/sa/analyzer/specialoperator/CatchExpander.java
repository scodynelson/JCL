package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.CatchStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchExpander extends MacroFunctionExpander<CatchStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.CATCH;
	}

	@Override
	public CatchStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "CATCH");

		final ListStruct formRest = form.getRest();

		final LispStruct catchTag = formRest.getCar();
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);

		final ListStruct formRestRest = formRest.getRest();

		final List<LispStruct> forms = formRestRest.getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new CatchStruct(catchTagAnalyzed, new PrognStruct(analyzedForms));
	}
}
