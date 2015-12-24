package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.MultipleValueCallStruct;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueCallExpander extends MacroFunctionExpander<MultipleValueCallStruct> {

	private static final long serialVersionUID = -8350781874747372684L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private FunctionExpander functionExpander;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.MULTIPLE_VALUE_CALL;
	}

	@Override
	public MultipleValueCallStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "MULTIPLE-VALUE-CALL");

		final ListStruct formRest = form.getRest();

		final LispStruct functionForm = formRest.getFirst();
		final LispStruct functionFormAnalyzed = formAnalyzer.analyze(functionForm, environment);

		final CompilerFunctionStruct functionFormAsCompilerFunction;
		if (functionFormAnalyzed instanceof CompilerFunctionStruct) {
			functionFormAsCompilerFunction = (CompilerFunctionStruct) functionFormAnalyzed;
		} else if (functionFormAnalyzed instanceof QuoteStruct) {
			final QuoteStruct quotedFunction = (QuoteStruct) functionFormAnalyzed;
			final ListStruct functionListStruct = ListStruct.buildProperList(SpecialOperatorStruct.FUNCTION, quotedFunction.getObject());
			functionFormAsCompilerFunction = functionExpander.expand(functionListStruct, environment);
		} else {
			final String printedObject = printer.print(functionForm);
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Invalid argument for function argument: " + printedObject);
		}

		final ListStruct formRestRest = formRest.getRest();

		final List<LispStruct> forms = formRestRest.getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new MultipleValueCallStruct(functionFormAsCompilerFunction, analyzedForms);
	}
}
