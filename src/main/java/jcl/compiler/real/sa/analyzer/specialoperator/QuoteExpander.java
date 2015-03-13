package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.QuoteStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuoteExpander extends MacroFunctionExpander<QuoteStruct> {

	private static final long serialVersionUID = 2741011595927247743L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.QUOTE.setMacroFunctionExpander(this);
	}

	@Override
	public QuoteStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct quotedObject = inputRest.getFirst();

		ListStruct analyzedElement = null;
		if (quotedObject instanceof SymbolStruct) {
			analyzedElement = analyzeQuoteSymbol((SymbolStruct<?>) quotedObject);
		} else if (quotedObject instanceof ListStruct) {
			analyzedElement = analyzeQuoteList((ListStruct) quotedObject);
		}

		final LispStruct elementToAnalyze;
		if (analyzedElement == null) {
			elementToAnalyze = quotedObject;
		} else {
			// If was ListStruct or SymbolStruct, wrap resulting form in Load-Time-Value.
			elementToAnalyze = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, analyzedElement);
		}

		final LispStruct element = formAnalyzer.analyze(elementToAnalyze, environment);

		return new QuoteStruct(element);
	}

	private static ListStruct analyzeQuoteList(final ListStruct element) {
		final SymbolStruct<?> listFnSym;
		if (element.isDotted()) {
			listFnSym = CommonLispSymbols.LIST_STAR;
		} else {
			listFnSym = CommonLispSymbols.LIST;
		}

		final List<? extends LispStruct> formJavaList = element.getAsJavaList();

		final List<LispStruct> transformedListForms = new ArrayList<>();
		transformedListForms.add(listFnSym);
		transformedListForms.addAll(formJavaList);

		return ListStruct.buildProperList(transformedListForms);
	}

	private static ListStruct analyzeQuoteSymbol(final SymbolStruct<?> element) {
		final SymbolStruct<?> symbolFnSym;
		final String symbolNameString;

		final PackageStruct symbolPackage = element.getSymbolPackage();
		if (symbolPackage != null) {
			symbolFnSym = CommonLispSymbols.FIND_SYMBOL;
			symbolNameString = symbolPackage.getName();
		} else {
			symbolFnSym = CommonLispSymbols.MAKE_SYMBOL;
			symbolNameString = element.getName();
		}

		final StringStruct symbolNameStringStruct = new StringStruct(symbolNameString);

		final List<LispStruct> symbolQuoteList = new ArrayList<>();
		symbolQuoteList.add(symbolFnSym);
		symbolQuoteList.add(symbolNameStringStruct);

		return ListStruct.buildProperList(symbolQuoteList);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
