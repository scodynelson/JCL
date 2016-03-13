package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
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

	@Autowired
	private FormAnalyzer formAnalyzer;

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
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // MULTIPLE-VALUE-CALL SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();
		final LispStruct functionForm = formAnalyzer.analyze(first, environment);

		final CompilerFunctionStruct functionFormAsCompilerFunction;
		if (functionForm instanceof CompilerFunctionStruct) {
			functionFormAsCompilerFunction = (CompilerFunctionStruct) functionForm;
		} else if (functionForm instanceof QuoteStruct) {
			final QuoteStruct quotedFunction = (QuoteStruct) functionForm;
			final ListStruct functionListStruct = ListStruct.buildProperList(SpecialOperatorStruct.FUNCTION, quotedFunction.getObject());
			functionFormAsCompilerFunction = functionExpander.expand(functionListStruct, environment);
		} else {
			final String printedObject = printer.print(functionForm);
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Invalid argument for function argument: " + printedObject);
		}

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new MultipleValueCallStruct(functionFormAsCompilerFunction, forms);
	}
}
