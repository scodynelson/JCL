package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.MultipleValueCallStruct;
import jcl.compiler.real.struct.specialoperator.QuoteStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueCallExpander extends MacroFunctionExpander<MultipleValueCallStruct> {

	private static final long serialVersionUID = -8350781874747372684L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private FunctionExpander functionExpander;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the multiple-value-call macro function and adds it to the special operator 'multiple-value-call'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.MULTIPLE_VALUE_CALL.setMacroFunctionExpander(this);
	}

	@Override
	public MultipleValueCallStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

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
