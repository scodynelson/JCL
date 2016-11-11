package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.MultipleValueCallStruct;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.SpecialOperatorStructImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueCallExpander extends MacroFunctionExpander<MultipleValueCallStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private FunctionExpander functionExpander;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.MULTIPLE_VALUE_CALL;
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
			final ListStruct functionListStruct = LispStructFactory.toProperList(SpecialOperatorStructImpl.FUNCTION, quotedFunction.getObject());
			functionFormAsCompilerFunction = functionExpander.expand(functionListStruct, environment);
		} else {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Invalid argument for function argument: " + functionForm);
		}

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new MultipleValueCallStruct(functionFormAsCompilerFunction, forms);
	}
}
