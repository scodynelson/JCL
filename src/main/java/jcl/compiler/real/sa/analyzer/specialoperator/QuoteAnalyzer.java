package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.LoadTimeValueElement;
import jcl.compiler.real.sa.element.QuoteElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class QuoteAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2741011595927247743L;

	@Autowired
	private LoadTimeValueAnalyzer loadTimeValueAnalyzer;

	@Override
	public QuoteElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final LispStruct element = input.getRest().getFirst();

		final ListStruct analyzedElement;
		if (element instanceof ListStruct) {
			analyzedElement = analyzeQuoteList(analyzer, (ListStruct) element, analysisBuilder);
		} else if (element instanceof SymbolStruct) {
			analyzedElement = analyzeQuoteSymbol((SymbolStruct) element);
		} else {
			return new QuoteElement(element);
		}

		// If was ListStruct or SymbolStruct, wrap resulting form in Load-Time-Value.
		final ListStruct loadTimeValueForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, analyzedElement);
		final LoadTimeValueElement loadTimeValueElement = loadTimeValueAnalyzer.analyze(analyzer, loadTimeValueForm, analysisBuilder);

		return new QuoteElement(loadTimeValueElement);
	}

	private static ListStruct analyzeQuoteList(final SemanticAnalyzer analyzer, final ListStruct element, final AnalysisBuilder analysisBuilder) {
		final SymbolStruct<?> listFnSym;
		if (element.isDotted()) {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST*").getSymbolStruct();
		} else {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST").getSymbolStruct();
		}

		final List<LispStruct> formJavaList = element.getAsJavaList();

		final List<LispStruct> transformedForms
				= formJavaList.stream()
				              .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				              .collect(Collectors.toList());

		final List<LispStruct> transformedListForms = new ArrayList<>(transformedForms.size() + 1);
		transformedListForms.add(listFnSym);
		transformedListForms.addAll(transformedForms);

		return ListStruct.buildProperList(transformedListForms);
	}

	private static ListStruct analyzeQuoteSymbol(final SymbolStruct<?> element) {
		final String symbolFunctionString;
		final String symbolNameString;

		final PackageStruct symbolPackage = element.getSymbolPackage();
		if (symbolPackage != null) {
			symbolFunctionString = "FIND-SYMBOL";
			symbolNameString = symbolPackage.getName();
		} else {
			symbolFunctionString = "MAKE-SYMBOL";
			symbolNameString = element.getName();
		}

		final SymbolStruct<?> symbolFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol(symbolFunctionString).getSymbolStruct();
		final StringStruct symbolNameStringStruct = new StringStruct(symbolNameString);

		final List<LispStruct> symbolQuoteList = new ArrayList<>(2);
		symbolQuoteList.add(symbolFnSym);
		symbolQuoteList.add(symbolNameStringStruct);

		return ListStruct.buildProperList(symbolQuoteList);
	}
}
